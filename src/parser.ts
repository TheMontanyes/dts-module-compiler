import ts from 'typescript';

import {
  ClassDeclarationDefinition,
  FunctionDeclarationDefinition,
  ParsedModule,
  ParsedNode,
  ReExportModule,
  VariableDeclarationDefinition,
} from './types';
import { createSearchLinkedNodesFromNode } from './search-linked-nodes';
import {
  createRegexpIdentifier,
  getJsDOC,
  isFromStdLib,
  isNodeFromPackage,
  replaceImport,
} from './helpers';
import {
  printClassDeclarationDefinition,
  printFunctionDeclarationDefinition,
  printVariableDeclarationDefinition,
} from './printer';

export const createSourceFileParser = (typeChecker: ts.TypeChecker) => {
  const collectionParsedNodes = new Map<ts.Node, ParsedNode>();
  const visitedLinkedNodes = new Set<string>();
  const searchLinkedNodesFromNode = createSearchLinkedNodesFromNode(typeChecker);

  // const foo = FooService.getValue
  function handlePropertyAccessExpression(
    expression: ts.PropertyAccessExpression,
    parsed: ParsedNode,
  ) {
    const type = typeChecker.getTypeAtLocation(expression);
    const node = type.symbol?.declarations?.[0];

    if (node) {
      if (ts.isFunctionLike(node)) {
        if ('body' in node && node.body) {
          if (ts.isExpression(node.body)) {
            handleReturnStatementExpression(node.body, parsed);
          }

          if (ts.isBlock(node.body)) {
            node.body.statements.forEach((statement) => {
              if (ts.isReturnStatement(statement)) {
                if (statement.expression) {
                  handleReturnStatementExpression(statement.expression, parsed);
                }
              }
            });
          }
        }
      } else {
        parsed.linkedNodes.push(node);
      }
    }
  }

  function handleObjectLiteralExpression(
    expression: ts.ObjectLiteralExpression,
    parsed: ParsedNode,
  ) {
    expression.properties.forEach((property) => {
      if (ts.isShorthandPropertyAssignment(property)) {
        const type = typeChecker.getTypeAtLocation(property);
        const node = type.symbol?.declarations?.[0];

        if (node) {
          if (!ts.isTypeNode(node)) {
            parsed.linkedNodes.push(node);
          }

          searchLinkedNodesFromNode(node).forEach((linkedNode) => {
            parsed.linkedNodes.push(linkedNode);
          });
        }
      }

      if (ts.isPropertyAssignment(property)) {
        if (ts.isObjectLiteralExpression(property.initializer)) {
          handleObjectLiteralExpression(property.initializer, parsed);
          return;
        }

        const type = typeChecker.getTypeAtLocation(property.initializer);
        const node = type.symbol?.declarations?.[0];

        if (ts.isPropertyAccessExpression(property.initializer)) {
          handlePropertyAccessExpression(property.initializer, parsed);
        }

        if (node) {
          searchLinkedNodesFromNode(node).forEach((linkedNode) => {
            parsed.linkedNodes.push(linkedNode);
          });
        }
      }
    });
  }

  function handleArrayLiteralExpression(expression: ts.ArrayLiteralExpression, parsed: ParsedNode) {
    expression.elements.forEach((element) => {
      if (ts.isIdentifier(element)) {
        const type = typeChecker.getTypeAtLocation(element);
        const linkedNode = type.symbol?.declarations?.[0];

        if (linkedNode) {
          parsed.linkedNodes.push(linkedNode);
        }
      }
    });
  }

  function handleReturnStatementExpression(expression: ts.Expression, parsed: ParsedNode) {
    if (ts.isIdentifier(expression)) {
      const type = typeChecker.getTypeAtLocation(expression);
      const node = type.symbol?.declarations?.[0];

      if (node) {
        parsed.linkedNodes.push(
          ts.isTypeLiteralNode(node) && ts.isTypeAliasDeclaration(node.parent) ? node.parent : node,
        );
      }

      return;
    }

    if (ts.isArrayLiteralExpression(expression)) {
      handleArrayLiteralExpression(expression, parsed);
      return;
    }

    if (ts.isParenthesizedExpression(expression)) {
      handleReturnStatementExpression(expression.expression, parsed);
      return;
    }

    if (ts.isObjectLiteralExpression(expression)) {
      handleObjectLiteralExpression(expression, parsed);
      return;
    }

    if (ts.isFunctionLike(expression)) {
      handleFunctionLike(expression, parsed);
      return;
    }

    if (ts.isCallOrNewExpression(expression)) {
      handleCallOrNewExpression(expression, parsed);
      return;
    }

    if (ts.isPropertyAccessExpression(expression)) {
      handlePropertyAccessExpression(expression, parsed);

      return;
    }

    if (ts.isAsExpression(expression)) {
      if (expression.type) {
        const nodes = searchLinkedNodesFromNode(expression.type);

        if (nodes.length > 0) {
          parsed.linkedNodes.push(...nodes);
        }
      }

      return;
    }
  }

  function handleTypeParameter(typeParameter: ts.TypeParameterDeclaration, parsed: ParsedNode) {
    if (typeParameter.default) {
      const nodes = searchLinkedNodesFromNode(typeParameter.default);

      if (nodes.length > 0) {
        parsed.linkedNodes.push(...nodes);
      }
    }

    if (typeParameter.constraint) {
      const nodes = searchLinkedNodesFromNode(typeParameter.constraint);

      if (nodes.length > 0) {
        parsed.linkedNodes.push(...nodes);
      }
    }
  }

  function handleFunctionLike(declaration: ts.SignatureDeclaration, parsed: ParsedNode) {
    if ('body' in declaration && declaration.body) {
      if (ts.isExpression(declaration.body)) {
        handleReturnStatementExpression(declaration.body, parsed);
      }

      if (ts.isBlock(declaration.body)) {
        declaration.body.statements.forEach((statement) => {
          if (ts.isReturnStatement(statement)) {
            if (statement.expression) {
              handleReturnStatementExpression(statement.expression, parsed);
            }
          }
        });
      }
    }

    if (declaration.typeParameters?.length) {
      declaration.typeParameters.forEach((typeParameter) => {
        handleTypeParameter(typeParameter, parsed);
      });
    }

    if (declaration.parameters.length) {
      declaration.parameters.forEach((parameter) => {
        if (parameter.type) {
          const nodes = searchLinkedNodesFromNode(parameter.type);

          if (nodes.length > 0) {
            parsed.linkedNodes.push(...nodes);
          }
        }
      });
    }

    if (declaration.type) {
      const nodes = searchLinkedNodesFromNode(declaration.type);

      if (nodes.length > 0) {
        parsed.linkedNodes.push(...nodes);
      }
    }
  }

  function handleCallOrNewExpression(
    expression: ts.CallExpression | ts.NewExpression,
    parsed: ParsedNode,
  ) {
    const signature = typeChecker.getResolvedSignature(expression);

    if (signature) {
      const returnType = typeChecker.getReturnTypeOfSignature(signature);
      const symbol = returnType.getSymbol();

      if (symbol) {
        const { declarations } = symbol;

        if (declarations) {
          declarations.forEach((callDeclaration) => {
            if (!isNodeFromPackage(callDeclaration) && !isFromStdLib(callDeclaration)) {
              parsed.linkedNodes.push(callDeclaration);
            }

            if (ts.isFunctionLike(callDeclaration)) {
              const type = callDeclaration.type;

              if (type) {
                const nodes = searchLinkedNodesFromNode(type);

                if (nodes.length > 0) {
                  parsed.linkedNodes.push(...nodes);
                }
              }
            }
          });
        }
      }
    }
  }

  const parseVariableDeclaration = (declaration: ts.VariableDeclaration): ParsedNode => {
    const statement = declaration.parent.parent as ts.VariableStatement;

    const typeExpressionNode = typeChecker.getTypeAtLocation(declaration);
    const typeString = typeChecker.typeToString(
      typeExpressionNode,
      declaration,
      ts.TypeFormatFlags.NoTruncation,
    );

    const definition: VariableDeclarationDefinition = {
      identifierName: declaration.name.getText(),
      typeAnnotation: typeString,
      keyword: ts.ScriptElementKind.constElement,
      modifiers: statement.modifiers?.map((m) => m.getText()),
    };

    const parsed = {
      jsDoc: getJsDOC(declaration),
      name: definition.identifierName,
      code: printVariableDeclarationDefinition(definition),
      linkedNodes: [],
    } as ParsedNode;

    if (declaration.type) {
      const nodes = searchLinkedNodesFromNode(declaration.type);

      if (nodes.length > 0) {
        parsed.linkedNodes.push(...nodes);
      }
    } else {
      const expression = declaration.initializer;

      definition.typeAnnotation = replaceImport(definition.typeAnnotation);
      parsed.code = printVariableDeclarationDefinition(definition);

      if (expression) {
        if (ts.isAsExpression(expression)) {
          if (expression.type) {
            const nodes = searchLinkedNodesFromNode(expression.type);

            if (nodes.length > 0) {
              parsed.linkedNodes.push(...nodes);
            }
          }
        }

        if (ts.isCallOrNewExpression(expression)) {
          handleCallOrNewExpression(expression, parsed);
        }

        if (ts.isFunctionLike(expression)) {
          handleFunctionLike(expression, parsed);
        }

        // const foo = FooService.getValue
        if (ts.isPropertyAccessExpression(expression)) {
          handlePropertyAccessExpression(expression, parsed);
        }

        // const foo = {foo: {bar: ''}}
        if (ts.isObjectLiteralExpression(expression)) {
          handleObjectLiteralExpression(expression, parsed);
        }
      }
    }

    return parsed;
  };

  const parseFunctionDeclaration = (declaration: ts.FunctionDeclaration): ParsedNode => {
    const signature = typeChecker.getSignatureFromDeclaration(declaration);

    const returnType =
      declaration.type?.getText() ||
      (signature &&
        typeChecker.typeToString(
          signature.getReturnType(),
          signature.declaration,
          ts.TypeFormatFlags.NoTruncation,
        )) ||
      'any';

    const definition: FunctionDeclarationDefinition = {
      identifierName: declaration.name ? declaration.name.getText() : '__anonymous',
      parameters: [],
      returnType,
      keyword: ts.ScriptElementKind.functionElement,
      modifiers: declaration.modifiers?.map((m) => m.getText()),
      typeParameters: declaration.typeParameters
        ? parseTypeParameters(declaration.typeParameters)
        : '',
    };

    const parsed = {
      jsDoc: getJsDOC(declaration),
      name: definition.identifierName,
      code: printFunctionDeclarationDefinition(definition),
      linkedNodes: [],
    } as ParsedNode;

    handleFunctionLike(declaration, parsed);

    if (declaration.parameters.length) {
      declaration.parameters.forEach((parameter) => {
        definition.parameters.push(
          `${parameter.name.getText()}${typeChecker.isOptionalParameter(parameter) ? '?:' : ':'} ${
            parameter.type?.getText() ?? 'any'
          }`,
        );
      });
    }

    return parsed;
  };

  const parseVariableStatement = (statement: ts.VariableStatement) => {
    const [declaration] = statement.declarationList.declarations;
    return parseVariableDeclaration(declaration);
  };

  const parseTypeAliasDeclaration = (declaration: ts.TypeAliasDeclaration): ParsedNode => {
    const parsed: ParsedNode = {
      jsDoc: getJsDOC(declaration),
      name: declaration.name.getText(),
      code: declaration.getText().trim(),
      linkedNodes: [],
    };

    if (declaration.typeParameters?.length) {
      declaration.typeParameters.forEach((typeParameter) => {
        handleTypeParameter(typeParameter, parsed);
      });
    }

    if (declaration.type) {
      const nodes = searchLinkedNodesFromNode(declaration.type);

      if (nodes.length > 0) {
        parsed.linkedNodes.push(...nodes);
      }
    }

    return parsed;
  };

  const parseEnumDeclaration = (declaration: ts.EnumDeclaration): ParsedNode => {
    return {
      jsDoc: getJsDOC(declaration),
      name: declaration.name.getText(),
      code: declaration.getText().trim(),
      linkedNodes: [],
    };
  };

  const parseInterfaceDeclaration = (declaration: ts.InterfaceDeclaration): ParsedNode => {
    const parsed: ParsedNode = {
      jsDoc: getJsDOC(declaration),
      name: declaration.name.getText(),
      code: declaration.getText().trim(),
      linkedNodes: [],
    };

    if (declaration.typeParameters?.length) {
      declaration.typeParameters.forEach((typeParameter) => {
        handleTypeParameter(typeParameter, parsed);
      });
    }

    declaration.heritageClauses?.forEach((clause) => {
      clause.types.forEach((expression) => {
        const symbol = typeChecker.getSymbolAtLocation(expression.expression);

        if (symbol?.declarations) {
          symbol?.declarations.forEach((clauseDeclaration) => {
            if (clauseDeclaration) {
              parsed.linkedNodes.push(clauseDeclaration);
            }

            expression.typeArguments?.forEach((typeNode) => {
              const nodes = searchLinkedNodesFromNode(typeNode);

              if (nodes.length > 0) {
                parsed.linkedNodes.push(...nodes);
              }
            });
          });
        }
      });
    });

    declaration.members.forEach((member) => {
      if (ts.isFunctionLike(member)) {
        handleFunctionLike(member, parsed);
      }

      if (ts.isPropertySignature(member) && member.type) {
        const nodes = searchLinkedNodesFromNode(member.type);

        if (nodes.length > 0) {
          parsed.linkedNodes.push(...nodes);
        }
      }
    });

    return parsed;
  };

  const parseTypeParameters = (
    typeParameters: ts.TypeParameterDeclaration[] | ts.NodeArray<ts.TypeParameterDeclaration>,
  ) => {
    return `<${typeParameters.map((typeParameter) => typeParameter.getText()).join(', ')}>`;
  };

  const parseClassDeclaration = (declaration: ts.ClassDeclaration): ParsedNode => {
    const definition: ClassDeclarationDefinition = {
      keyword: ts.ScriptElementKind.classElement,
      heritageClauses: [],
      constructorParameters: [],
      modifiers: declaration.modifiers ? declaration.modifiers.map((m) => m.getText()) : [],
      members: [],
      identifierName: declaration.name?.getText() || '__anonymous',
      typeParameters: declaration.typeParameters
        ? parseTypeParameters(declaration.typeParameters)
        : '',
    };

    const parsed: ParsedNode = {
      jsDoc: getJsDOC(declaration),
      linkedNodes: [],
      code: '',
      name: definition.identifierName,
    };

    if (declaration.typeParameters?.length) {
      declaration.typeParameters.forEach((typeParameter) => {
        handleTypeParameter(typeParameter, parsed);
      });
    }

    declaration.heritageClauses?.forEach((clause) => {
      definition.heritageClauses.push(clause.getText());

      clause.types.forEach((expression) => {
        const symbol = typeChecker.getSymbolAtLocation(expression.expression);

        if (symbol?.declarations?.length) {
          symbol.declarations.forEach((clauseDeclaration) => {
            if (clauseDeclaration) {
              parsed.linkedNodes.push(clauseDeclaration);
            }

            expression.typeArguments?.forEach((typeNode) => {
              const nodes = searchLinkedNodesFromNode(typeNode);

              if (nodes.length > 0) {
                parsed.linkedNodes.push(...nodes);
              }
            });
          });
        }
      });
    });

    definition.members = declaration.members.reduce((acc, member) => {
      if (ts.isFunctionLike(member)) {
        handleFunctionLike(member, parsed);
      }

      if (ts.isConstructorDeclaration(member)) {
        definition.constructorParameters = member.parameters.map((parameter) => {
          const text = `${
            parameter.modifiers
              ? parameter.modifiers.reduce((acc, m) => (acc += `${m.getText()} `), '')
              : ''
          }${parameter.name.getText()}${typeChecker.isOptionalParameter(parameter) ? '?:' : ':'} ${
            parameter.type?.getText() ?? 'any'
          }`;

          return text;
        });

        return acc;
      }

      if (ts.isMethodDeclaration(member) || ts.isAccessor(member)) {
        const signature = typeChecker.getSignatureFromDeclaration(member);
        const signatureReturnType = signature?.getReturnType();
        const signatureReturnTypeString =
          signature &&
          signatureReturnType &&
          typeChecker.typeToString(
            signatureReturnType,
            signature.declaration,
            ts.TypeFormatFlags.NoTruncation,
          );

        const returnType = member.type?.getText() || signatureReturnTypeString || 'any';

        if (signatureReturnType?.symbol) {
          const declarations = signatureReturnType.symbol.getDeclarations();

          if (declarations?.length) {
            declarations.forEach((declaration) => {
              if (ts.isTypeNode(declaration)) {
                const nodes = searchLinkedNodesFromNode(declaration);

                if (nodes.length > 0) {
                  parsed.linkedNodes.push(...nodes);
                }
              }
            });
          }
        }

        let resultMember = `${member.name.getText()}`;

        if (member.typeParameters?.length) {
          resultMember += `<${member.typeParameters
            .map((parameter) => parameter.getText())
            .join(', ')}>`;
        }

        const parameters: string[] = [];

        if (member.parameters.length) {
          member.parameters.forEach((parameter) => {
            parameters.push(
              `${parameter.name.getText()}${
                typeChecker.isOptionalParameter(parameter) ? '?:' : ':'
              } ${parameter.type?.getText() ?? 'any'}`,
            );

            if (parameter.type) {
              const nodes = searchLinkedNodesFromNode(parameter.type);

              if (nodes.length > 0) {
                parsed.linkedNodes.push(...nodes);
              }
            }
          });
        }

        resultMember += `(${parameters.join(', ')}): ${returnType}`;

        if (ts.isGetAccessor(member)) {
          resultMember = `get ${resultMember}`;
        }

        if (ts.isSetAccessor(member)) {
          resultMember = `set ${resultMember}`;
        }

        if (member.modifiers?.length) {
          resultMember = `${member.modifiers.reduce(
            (acc, currentValue) => (acc += `${currentValue.getText()} `),
            '',
          )}${resultMember}`;
        }

        const doc = getJsDOC(member);

        if (doc) {
          resultMember = `${doc}${ts.sys.newLine}${resultMember}`;
        }

        acc.push(resultMember);

        return acc;
      }

      if (ts.isPropertyDeclaration(member)) {
        let resultMember = ``;
        const type = typeChecker.getTypeAtLocation(member.name);
        const typeStringMember = typeChecker.typeToString(
          type,
          member.name,
          ts.TypeFormatFlags.NoTruncation,
        );

        if (member.initializer) {
          if (ts.isCallOrNewExpression(member.initializer)) {
            handleCallOrNewExpression(member.initializer, parsed);
          }

          if (ts.isFunctionLike(member.initializer)) {
            handleFunctionLike(member.initializer, parsed);
          }

          // const foo = FooService.getValue
          if (ts.isPropertyAccessExpression(member.initializer)) {
            handlePropertyAccessExpression(member.initializer, parsed);
          }
        }

        if (member.type) {
          const nodes = searchLinkedNodesFromNode(member.type);

          if (nodes.length > 0) {
            parsed.linkedNodes.push(...nodes);
          }
        }

        if (ts.canHaveModifiers(member) && member.modifiers?.length) {
          resultMember += member.modifiers.reduce((acc, modifier) => {
            acc += modifier.getText() + ' ';
            return acc;
          }, '');
        }

        resultMember += `${member.name?.getText()}: ${replaceImport(typeStringMember)}`;

        const doc = getJsDOC(member);

        if (doc) {
          resultMember = `${doc}${ts.sys.newLine}${resultMember}`;
        }

        acc.push(resultMember);

        return acc;
      }

      // let resultMember = ``;
      //
      // const type = typeChecker.getTypeAtLocation(member);
      // const typeStringMember = typeChecker.typeToString(
      //   type,
      //   member,
      //   ts.TypeFormatFlags.NoTruncation,
      // );
      //
      // if (ts.canHaveModifiers(member) && member.modifiers?.length) {
      //   resultMember += member.modifiers.reduce((acc, modifier) => {
      //     acc += modifier.getText() + ' ';
      //     return acc;
      //   }, '');
      // }
      //
      // resultMember += `${member.name?.getText()}: ${typeStringMember}`;
      //
      // const doc = getJsDOC(member);
      //
      // if (doc) {
      //   resultMember = `${doc}${ts.sys.newLine}${resultMember}`;
      // }
      //
      // acc.push(resultMember);

      return acc;
    }, definition.members);

    parsed.code = printClassDeclarationDefinition(definition);

    return parsed;
  };

  const parseNode = (node: ts.Node): ParsedNode | undefined => {
    if (isFromStdLib(node) || isNodeFromPackage(node)) {
      return;
    }

    if (collectionParsedNodes.has(node)) return collectionParsedNodes.get(node);

    const parsed = (() => {
      if (ts.isVariableStatement(node)) {
        return parseVariableStatement(node);
      }

      if (ts.isVariableDeclaration(node)) {
        return parseVariableDeclaration(node);
      }

      if (ts.isFunctionDeclaration(node)) {
        return parseFunctionDeclaration(node);
      }

      if (ts.isTypeAliasDeclaration(node)) {
        return parseTypeAliasDeclaration(node);
      }

      if (ts.isInterfaceDeclaration(node)) {
        return parseInterfaceDeclaration(node);
      }

      if (ts.isEnumDeclaration(node)) {
        return parseEnumDeclaration(node);
      }

      if (ts.isClassDeclaration(node)) {
        return parseClassDeclaration(node);
      }

      if (ts.isImportSpecifier(node)) {
        const importSymbol = typeChecker.getSymbolAtLocation(node.name);

        if (importSymbol) {
          const aliasedSymbol = typeChecker.getAliasedSymbol(importSymbol);

          if (aliasedSymbol.declarations) {
            return parseNode(aliasedSymbol.valueDeclaration || aliasedSymbol.declarations[0]);
          }
        }
      }

      return;
    })();

    if (parsed) {
      collectionParsedNodes.set(node, parsed);
    }

    return parsed;
  };

  const parseLinkedNodes = (parsedNode: ParsedNode): ParsedNode[] => {
    const keyVisited = `${parsedNode.name}_${parsedNode.linkedNodes.length}`;

    if (parsedNode.linkedNodes.length === 0 || visitedLinkedNodes.has(keyVisited)) return [];

    visitedLinkedNodes.add(keyVisited);

    return [
      ...parsedNode.linkedNodes.reduce((acc, node) => {
        const parsed = parseNode(node);

        if (ts.isImportSpecifier(node)) {
          const importSymbol = typeChecker.getSymbolAtLocation(node.name);

          if (importSymbol) {
            const aliasedSymbol = typeChecker.getAliasedSymbol(importSymbol);

            if (aliasedSymbol.declarations) {
              aliasedSymbol.declarations.forEach((importedNode) => {
                if (isNodeFromPackage(importedNode)) {
                  parsedNode.code = parsedNode.code.replace(
                    createRegexpIdentifier(importSymbol.name),
                    `import(${node.parent.parent.parent.moduleSpecifier.getText()}).${
                      node.propertyName ? node.propertyName.getText() : node.name.text
                    }`,
                  );
                }
              });
            }
          }
        } else if (isNodeFromPackage(node)) {
          if (ts.isModuleBlock(node.parent)) {
            const moduleSymbol = typeChecker.getSymbolAtLocation(node.parent.getSourceFile());
            const modulePath = moduleSymbol?.name;

            if (modulePath) {
              parsedNode.code = parsedNode.code.replace(
                createRegexpIdentifier(node.parent.parent.name.getText()),
                replaceImport(`import(${modulePath})`),
              );
            }
          }
        }

        if (parsed) {
          acc.add(parsed);

          if (parsed.linkedNodes.length > 0) {
            parseLinkedNodes(parsed).forEach((node) => {
              acc.add(node);
            });
          }
        }

        return acc;
      }, new Set<ParsedNode>()),
    ];
  };

  return (sourceFile: ts.SourceFile): ParsedModule => {
    const module = sourceFile.statements.reduce(
      (acc, statement) => {
        if (ts.isExportDeclaration(statement)) {
          statement.exportClause?.forEachChild((node) => {
            if (ts.isExportSpecifier(node)) {
              const isTypeOnly = node.isTypeOnly || statement.isTypeOnly;
              const hasAsName = Boolean(node.propertyName);

              const nodeName = node.name.getText();

              const symbol = typeChecker.getSymbolAtLocation(node.name);

              if (symbol) {
                const aliasedSymbol = typeChecker.getAliasedSymbol(symbol);
                const declaration = aliasedSymbol.declarations?.[0];

                if (!declaration || isNodeFromPackage(declaration)) {
                  const unknownSymbol = (sourceFile as any).locals.get(
                    hasAsName ? node.propertyName!.getText() : nodeName,
                  );

                  if (unknownSymbol?.declarations?.[0]) {
                    const externalImportModule = unknownSymbol.declarations[0];

                    if (ts.isNamespaceImport(externalImportModule)) {
                      const packageName =
                        externalImportModule.parent.parent.moduleSpecifier.getText();

                      const reExportItem: ReExportModule = {
                        identifierNameImport: externalImportModule.name.getText(),
                        identifierNameExport: externalImportModule.name.getText(),
                        isNameSpaceImport: true,
                        isTypeOnlyExport: isTypeOnly,
                        asNameExport: hasAsName ? nodeName : undefined,
                      };

                      if (!acc.reExportsFromExternalModules.has(packageName)) {
                        acc.reExportsFromExternalModules.set(packageName, [reExportItem]);
                      } else {
                        acc.reExportsFromExternalModules.get(packageName)!.push(reExportItem);
                      }

                      return;
                    }

                    if (ts.isImportClause(externalImportModule)) {
                      if (externalImportModule.name) {
                        const packageName = externalImportModule.parent.moduleSpecifier.getText();

                        const reExportItem: ReExportModule = {
                          identifierNameImport: externalImportModule.name.getText(),
                          identifierNameExport: externalImportModule.name.getText(),
                          isDefaultImport: true,
                          isTypeOnlyExport: isTypeOnly,
                          asNameExport: hasAsName ? nodeName : undefined,
                        };

                        if (!acc.reExportsFromExternalModules.has(packageName)) {
                          acc.reExportsFromExternalModules.set(packageName, [reExportItem]);
                        } else {
                          acc.reExportsFromExternalModules.get(packageName)!.push(reExportItem);
                        }

                        return;
                      }
                    }

                    if (ts.isImportSpecifier(externalImportModule)) {
                      const packageName =
                        externalImportModule.parent.parent.parent.moduleSpecifier.getText();

                      const reExportItem: ReExportModule = {
                        identifierNameExport: externalImportModule.name.getText(),
                        identifierNameImport: externalImportModule.propertyName
                          ? externalImportModule.propertyName.getText()
                          : externalImportModule.name.getText(),
                        isTypeOnlyExport: isTypeOnly,
                        asNameImport: externalImportModule.propertyName
                          ? externalImportModule.name.getText()
                          : undefined,
                        asNameExport: hasAsName ? nodeName : undefined,
                      };

                      if (!acc.reExportsFromExternalModules.has(packageName)) {
                        acc.reExportsFromExternalModules.set(packageName, [reExportItem]);
                      } else {
                        acc.reExportsFromExternalModules.get(packageName)!.push(reExportItem);
                      }

                      return;
                    }
                  }
                }

                if (declaration) {
                  const parsedNode = parseNode(declaration);

                  if (parsedNode) {
                    const parsedLinkedNodes = parseLinkedNodes(parsedNode);

                    parsedLinkedNodes.forEach((linkedNode) => {
                      acc.linkedParsedNodes.add(linkedNode);
                    });

                    if (isTypeOnly) {
                      acc.linkedParsedNodes.add(parsedNode);
                      acc.exportedIdentifiersTypeOnly.add(node.name.getText());
                    } else {
                      if (hasAsName) {
                        parsedNode.code = parsedNode.code.replace(
                          new RegExp(`\\b${parsedNode.name}\\b`),
                          nodeName,
                        );
                      }

                      acc.exportedParsedNodes.add(parsedNode);
                    }
                  }
                }
              }
            }
          });
        }

        // export default
        if (ts.isExportAssignment(statement)) {
          const typeString = typeChecker.typeToString(
            typeChecker.getTypeAtLocation(statement.expression),
            statement.expression,
            ts.TypeFormatFlags.NoTruncation,
          );

          acc.exportDefaultParsedNode = {
            jsDoc: '',
            code: `const _default: ${replaceImport(typeString)};${
              ts.sys.newLine
            }export default _default;`,
            name: '',
            linkedNodes: [],
          };
        }

        if (ts.canHaveModifiers(statement)) {
          const isExport = statement.modifiers?.some(
            (modifier) => modifier.kind === ts.SyntaxKind.ExportKeyword,
          );

          if (isExport) {
            const parsedNode = parseNode(statement);

            if (parsedNode) {
              const parsedLinkedNodes = parseLinkedNodes(parsedNode);

              parsedLinkedNodes.forEach((linkedNode) => {
                acc.linkedParsedNodes.add(linkedNode);
              });

              acc.exportedParsedNodes.add(parsedNode);
            }
          }
        }

        return acc;
      },
      {
        exportedIdentifiersTypeOnly: new Set(),
        linkedParsedNodes: new Set(),
        exportedParsedNodes: new Set(),
        reExportsFromExternalModules: new Map(),
      } as ParsedModule,
    );

    // Clear visited linked nodes for every new module
    visitedLinkedNodes.clear();

    return module;
  };
};
