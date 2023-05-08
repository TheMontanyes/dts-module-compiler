import ts from 'typescript';

import { Definition, Import, SourceFileWithLocals } from '../types';
import { KEYWORDS_KEYS, KEYWORDS } from './keywords';
import { isImportExternalModule } from './isImportExternalModule';
import { getRealSymbol } from './getRealSymbol';
import { getSourceFileFromNode } from './getSourceFileFromNode';
import { getPathsToSrcFilesFromType } from './getPathsToSrcFilesFromType';
import { replaceImport } from './replaceImport';
import { isCustomType } from './isCustomType';
import { createRegexpIdentifier } from './createRegexpIdentifier';
import { getGenericsTypes } from './getGenericsTypes';
import { getMemberType } from './getMemberType';
import { replaceImportIdentifier } from './replaceImportIdentifier';
import { replaceDoubleImport } from './replaceDoubleImport';

type FunctionLike =
  | ts.FunctionDeclaration
  | ts.ArrowFunction
  | ts.FunctionExpression
  | ts.MethodDeclaration;

const isFunctionLike = (node: ts.Node): node is FunctionLike => {
  return (
    ts.isFunctionDeclaration(node) ||
    ts.isArrowFunction(node) ||
    ts.isFunctionExpression(node) ||
    ts.isMethodDeclaration(node)
  );
};

export class ConstructType {
  visitedNodes = new Set<ts.Symbol>();
  usedLocals = new Map<string, ts.Symbol>();
  private getUsedLocals(string: string, srcFile: SourceFileWithLocals) {
    const usedImports: Import[] = [];
    const usedLocals = new Map<string, ts.Symbol>();

    srcFile.statements.forEach((statement) => {
      if (ts.isImportDeclaration(statement)) {
        const importsFromImportDeclaration = this.getImportsFromImportDeclaration(statement);

        usedImports.push(...importsFromImportDeclaration);
      }
    });

    srcFile.locals.forEach((locale) => {
      const nameIdentifierRegexp = createRegexpIdentifier(locale.name);

      const nodes = locale.declarations;
      if (nodes) {
        nodes.forEach((node) => {
          if (ts.isImportSpecifier(node)) {
            if (node.parent.parent.parent && ts.isImportDeclaration(node.parent.parent.parent))
              usedImports.push(...this.getImportsFromImportDeclaration(node.parent.parent.parent));
          }
        });
      }

      if (nameIdentifierRegexp.test(string)) {
        const isImportExternal = usedImports.some(
          (imp) => imp.isExternal && (imp.asName ?? imp.name) === locale.name,
        );
        if (!usedLocals.has(locale.name) && !isImportExternal) {
          usedLocals.set(locale.name, locale);
        }
      }
    });

    return [usedLocals, usedImports] as const;
  }

  constructor(private program: ts.Program, private typeChecker: ts.TypeChecker) {}

  getImportsFromImportDeclaration(importDeclaration: ts.ImportDeclaration): Import[] {
    const imports: Import[] = [];
    const importPath = importDeclaration.moduleSpecifier.getText();
    const isExternal = isImportExternalModule(importPath, this.program.getCompilerOptions());
    // TODO-ARKHIPOV: проверить типизацию SVG
    const isSVG = Boolean(importPath.match(/['"].*\.svg["']/));

    if (importDeclaration.importClause?.namedBindings) {
      const bindings = importDeclaration.importClause.namedBindings;
      if (ts.isNamedImports(bindings)) {
        bindings.elements.forEach((node) => {
          const name = node.name.getText();

          imports.push({
            name: isExternal && node.propertyName ? node.propertyName.getText() : name,
            asName: isExternal
              ? node?.propertyName?.getText() && name
              : node?.propertyName?.getText(),
            from: importPath,
            isExternal,
            isSVG,
            isDefault: false,
          });
        });
      }
    }

    const defaultName = importDeclaration.importClause?.name?.getText();

    if (defaultName) {
      imports.push({
        name: defaultName,
        isDefault: true,
        isExternal,
        isSVG,
        from: importPath,
      });
    }

    return imports;
  }

  getObjectLiteralFromObjectLiteralExpression = (literal: ts.ObjectLiteralExpression) => {
    let result = '{';
    let isSpread = false;
    const intersections = new Set<string>();

    literal.forEachChild((node) => {
      if (ts.isPropertyAssignment(node)) {
        const propertyName = node.name.getText();

        if (ts.isIdentifier(node.initializer)) {
          const expression = node.initializer;
          const type = this.typeChecker.getTypeAtLocation(expression);
          const typeStr = this.typeChecker.typeToString(
            type,
            expression,
            ts.TypeFormatFlags.NoTruncation,
          );

          result += `${propertyName}: ${typeStr};\n`;
        } else if (ts.isObjectLiteralExpression(node.initializer)) {
          result += `${propertyName}: ${this.getObjectLiteralFromObjectLiteralExpression(
            node.initializer,
          )};\n`;
        } else {
          const type = this.typeChecker.getTypeAtLocation(node);
          const typeStr = this.typeChecker.typeToString(
            type,
            node,
            ts.TypeFormatFlags.NoTruncation,
          );

          result += `${propertyName}: ${typeStr};\n`;
        }
      }

      if (ts.isShorthandPropertyAssignment(node)) {
        const propertyName = node.name.getText();
        const typeNode = this.typeChecker.getTypeAtLocation(node.name);
        const typeStr = this.typeChecker.typeToString(
          typeNode,
          node.name,
          ts.TypeFormatFlags.NoTruncation,
        );

        result += `${propertyName}: ${typeStr};\n`;
      }

      // TODO-ARKHIPOV: сделать нормальную реализацию с SpreadAssignment
      if (ts.isSpreadAssignment(node)) {
        isSpread = true;
        const typeNode = this.typeChecker.getTypeAtLocation(node.expression);
        const typeStr = this.typeChecker.typeToString(
          typeNode,
          node.expression,
          ts.TypeFormatFlags.NoTruncation,
        );

        intersections.add(`(${typeStr})`);
      }
    });

    result += '}';

    if (isSpread && intersections.size > 0) {
      result += `& ${[...intersections].join(' & ')}`;
    }

    return result;
  };

  getCallExpressionType(expression: ts.CallExpression) {
    const typeExpressionNode = this.typeChecker.getTypeAtLocation(expression);
    const signature = this.typeChecker.getResolvedSignature(expression);

    if (signature) {
      const returnType = this.typeChecker.getReturnTypeOfSignature(signature);
      const symbol = getRealSymbol(
        returnType.aliasSymbol ?? returnType.symbol ?? returnType.getSymbol(),
        this.typeChecker,
      );
      const typeString = this.typeChecker.typeToString(
        returnType,
        expression,
        ts.TypeFormatFlags.NoTruncation,
      );

      if (symbol?.getName()) {
        const srcFile = getSourceFileFromNode(expression);
        const paths = getPathsToSrcFilesFromType(typeString);
        const typeStringReplaced = replaceImport(typeString);

        const addSymbolToLocal = (symbol: ts.Symbol) => {
          const symbolName = symbol.getName();

          if (
            symbol &&
            isCustomType(symbol, this.program) &&
            !/[\\/]node_modules[\\/]/.test(srcFile.fileName) &&
            createRegexpIdentifier(symbolName).test(typeStringReplaced)
          ) {
            if (!this.usedLocals.has(symbolName)) {
              this.usedLocals.set(symbolName, symbol);
            }
          }
        };

        if (paths.length > 0) {
          const srcFiles = paths.map(this.program.getSourceFile);
          if (srcFiles.length > 0) {
            const locals = srcFiles.flatMap(
              (file) => this.getUsedLocals(typeStringReplaced, file as SourceFileWithLocals)[0],
            );

            if (locals.length > 0) {
              locals.forEach((localMap) => {
                if (localMap.size > 0) {
                  localMap.forEach((localSymbol) => {
                    addSymbolToLocal(localSymbol);
                  });
                }
              });
            }
          }
        }

        addSymbolToLocal(symbol);

        return typeString;
      }
    }

    return this.typeChecker.typeToString(
      typeExpressionNode,
      expression,
      ts.TypeFormatFlags.NoTruncation,
    );
  }

  getFunctionType(decl: FunctionLike, name?: string) {
    const typeNode = this.typeChecker.getTypeAtLocation(decl);

    const parameterTypes = this.getParameterTypes(decl).join(', ');

    let generics = '';

    if (decl.typeParameters && decl.typeParameters.length > 0) {
      generics += getGenericsTypes(decl.typeParameters);
    }

    let returnType = '';

    const getTypeExpression = (expression: ts.Expression) => {
      if (ts.isArrayLiteralExpression(expression)) {
        const arrayType = new Set<string>();

        expression.elements.forEach((exItem) => {
          if (ts.isSpreadElement(exItem)) {
            const exp = exItem.expression;
            const symbolAtLocation = this.typeChecker.getSymbolAtLocation(exp);

            if (symbolAtLocation?.declarations) {
              const {
                declarations: [declExp],
              } = symbolAtLocation;

              if (ts.isVariableDeclaration(declExp)) {
                const spreadArray = declExp.initializer as ts.ArrayLiteralExpression;

                spreadArray.elements.forEach((spreadExpression) => {
                  arrayType.add(getTypeExpression(spreadExpression));
                });
              }
            }

            return;
          }

          arrayType.add(getTypeExpression(exItem));
        });

        return `(${[...arrayType].filter(Boolean).join(' | ')})[]`;
      }

      if (ts.isIdentifier(expression)) {
        const typeNode = this.typeChecker.getTypeAtLocation(expression);
        return this.typeChecker.typeToString(typeNode, expression, ts.TypeFormatFlags.NoTruncation);
      }

      if (ts.isObjectLiteralExpression(expression)) {
        return this.getObjectLiteralFromObjectLiteralExpression(expression);
      }

      if (ts.isCallExpression(expression)) {
        return this.getCallExpressionType(expression);
      }

      if (isFunctionLike(expression)) {
        return this.getFunctionType(expression);
      }

      const typeNode = this.typeChecker.getTypeAtLocation(expression);
      return this.typeChecker.typeToString(typeNode, expression, ts.TypeFormatFlags.NoTruncation);
    };

    if (decl.body && decl.body.getChildCount() > 0 && ts.isBlock(decl.body)) {
      decl.body.forEachChild((node) => {
        if (ts.isReturnStatement(node)) {
          const { expression } = node;

          if (expression) {
            returnType = getTypeExpression(expression);
          }
        }
      });
    } else if (decl.body) {
      returnType = getTypeExpression(decl.body as ts.Expression);
    }

    if (decl.type && !returnType) {
      returnType = decl.type.getText();
    }

    if (!returnType && decl) {
      const signatureFromDeclaration = this.typeChecker.getSignatureFromDeclaration(decl);

      if (signatureFromDeclaration) {
        const type = this.typeChecker.getReturnTypeOfSignature(signatureFromDeclaration);
        returnType = this.typeChecker.typeToString(type, decl, ts.TypeFormatFlags.NoTruncation);
      }
    }

    const isPromiseReturned = typeNode.getCallSignatures().some((signature) => {
      const typeSignature = signature.getReturnType();

      return typeSignature.symbol && typeSignature.symbol.name === 'Promise';
    });

    if (isPromiseReturned && !/^Promise<(?!Promise)?.*>/.test(returnType)) {
      returnType = `Promise<${returnType}>`;
    }

    if (ts.isMethodDeclaration(decl)) return `${generics}(${parameterTypes}): ${returnType}`;

    if (ts.isFunctionDeclaration(decl)) {
      const functionName = (name || decl?.name?.getText()) ?? '';

      return `function ${functionName}${generics}(${parameterTypes}): ${returnType}`;
    }

    return `${generics}(${parameterTypes}) => ${returnType}`;
  }

  getParameterTypes<N extends ts.FunctionLikeDeclarationBase>(node?: N): string[] {
    if (!node) return [];

    return node.parameters.map((parameter) => {
      const typeParameter = this.typeChecker.getTypeAtLocation(parameter);
      const type = this.typeChecker.typeToString(
        typeParameter,
        parameter,
        ts.TypeFormatFlags.NoTruncation,
      );
      const isOptional = this.typeChecker.isOptionalParameter(parameter);

      const parameterName = parameter.name
        .getText()
        .replace(/(?=[{\[,\s])(.*)=.*(?=[}\],])/gm, '$1');

      return `${parameterName}${isOptional ? '?' : ''}: ${type}`;
    });
  }

  getModifiers(member: ts.ClassDeclaration | ts.MethodDeclaration | ts.PropertyDeclaration) {
    // static async getName() {}
    // member.modifiers === [static, async]
    return member?.modifiers
      ?.reduce((acc: string[], currModifier) => {
        const text = currModifier.getText();

        if (!['async', 'export'].includes(text)) {
          acc.push(text);
        }

        return acc;
      }, [])
      .join(' ');
  }

  getHeritageClausesType(clauses: ts.NodeArray<ts.HeritageClause>): string {
    const heritageClauses = clauses.map((value) => {
      return value.getText();
    });

    return ` ${heritageClauses.join(' ')} `;
  }

  getTypeFromClassDeclaration(decl: ts.ClassDeclaration, name?: string) {
    let result = `${KEYWORDS[ts.SyntaxKind.ClassDeclaration]} ${name ?? decl.name?.getText()}`;

    const modifiers = this.getModifiers(decl);

    if (modifiers) {
      result = `${modifiers} ${result}`;
    }

    if (decl.typeParameters && decl.typeParameters.length > 0) {
      result += getGenericsTypes(decl.typeParameters);
    }

    if (decl.heritageClauses && decl.heritageClauses.length > 0) {
      result += this.getHeritageClausesType(decl.heritageClauses);
    }

    result += '{';

    decl.members.forEach((member) => {
      const typeProp = this.typeChecker.getTypeAtLocation(member);
      const typePropString = this.typeChecker.typeToString(
        typeProp,
        decl,
        ts.TypeFormatFlags.NoTruncation,
      );
      const memberName = member.name?.getText();

      if (ts.isConstructorDeclaration(member)) {
        const parameterTypes = this.getParameterTypes(member).join(', ');
        result += `\nconstructor(${parameterTypes});`;
      }

      if (ts.isGetAccessor(member)) {
        result += `\nget ${memberName}(): ${typePropString};`;
      }

      if (ts.isSetAccessor(member)) {
        const parameterTypes = this.getParameterTypes(member).join(', ');
        const accessorType = `(${parameterTypes}): ${this.typeChecker.typeToString(
          typeProp,
          decl,
          ts.TypeFormatFlags.NoTruncation,
        )};`;

        result += `\nset ${memberName}${accessorType}`;
      }

      if (ts.isMethodDeclaration(member)) {
        const method = [];

        const modifier = this.getModifiers(member);

        if (modifier) {
          method.push(modifier);
        }

        method.push(memberName);

        result += `\n${method.join(' ')}${this.getFunctionType(member)};`;
      }

      if (ts.isPropertyDeclaration(member)) {
        const modifier = this.getModifiers(member);

        let propertyText = '';

        if (modifier) {
          propertyText += `${modifier} `;
        }

        if (member.initializer && ts.isNewExpression(member.initializer)) {
          const symbol = this.typeChecker.getSymbolAtLocation(member.initializer.expression);
          if (symbol?.declarations) {
            const node = symbol.declarations[0];
            const type = this.typeChecker.getTypeOfSymbolAtLocation(symbol, node);
            const returnType = type.getConstructSignatures()[0].getReturnType();
            const realSymbol = getRealSymbol(returnType.symbol, this.typeChecker);
            if (realSymbol && isCustomType(realSymbol, this.program)) {
              if (!this.usedLocals.has(returnType.symbol.name)) {
                this.usedLocals.set(returnType.symbol.name, returnType.symbol);
              }
            }
          }
        }

        propertyText += `${memberName}: ${typePropString}`;

        result += `\n${propertyText}`;
      }
    });

    result += '\n}';

    return result;
  }

  getTypeFromTypeAliasDeclaration(decl: ts.TypeAliasDeclaration, name?: string) {
    let result = `${KEYWORDS[ts.SyntaxKind.TypeAliasDeclaration]} ${name ?? decl.name.getText()}`;

    if (decl.typeParameters && decl.typeParameters.length > 0) {
      result += getGenericsTypes(decl.typeParameters);
    }

    result += ' = ';

    let isTypeLiteral = false;

    decl.forEachChild((node) => {
      if (ts.isTypeLiteralNode(node)) {
        isTypeLiteral = true;
        result += '{';

        node.members.forEach((member) => {
          result += getMemberType(member);
        });

        result += '\n}';
      }
    });

    if (!isTypeLiteral) {
      const expression = decl.type.getText();
      result += expression;
    }

    return result;
  }

  getTypesDefinitions(symbol: ts.Symbol, moduleName: string): Definition[] {
    const initialNode = (symbol.declarations ?? [])[0];

    if (
      ts.isImportSpecifier(initialNode) &&
      isImportExternalModule(
        initialNode.parent.parent.parent.moduleSpecifier.getText(),
        this.program.getCompilerOptions(),
      )
    ) {
      return [];
    }

    const realSymbol = getRealSymbol(symbol, this.typeChecker);

    if (realSymbol) {
      this.visitedNodes.add(symbol);
      this.visitedNodes.add(realSymbol);

      const getTypeFromNodeExpression = (node: ts.Node, definition: Definition) => {
        const type = this.typeChecker.getTypeAtLocation(node);
        let typeString = this.typeChecker.typeToString(type, node, ts.TypeFormatFlags.NoTruncation);

        if (ts.isInterfaceDeclaration(node) || ts.isEnumDeclaration(node)) {
          typeString = node
            .getText()
            .replace(new RegExp(`export ${definition.keyword}`), definition.keyword);

          typeString = typeString.replace(
            new RegExp(`${definition.keyword} ${node.name.getText()}`),
            `${definition.keyword} ${definition.name}`,
          );
        }

        if (isFunctionLike(node)) {
          typeString = this.getFunctionType(node, definition.name);
        }

        if (ts.isClassDeclaration(node)) {
          typeString = this.getTypeFromClassDeclaration(node, definition.name);
        }

        if (ts.isTypeAliasDeclaration(node)) {
          typeString = this.getTypeFromTypeAliasDeclaration(node, definition.name);
        }

        if (ts.isVariableDeclaration(node) && node.initializer) {
          if (ts.isCallExpression(node.initializer)) {
            typeString = this.getCallExpressionType(node.initializer);
          }

          if (ts.isArrowFunction(node.initializer)) {
            typeString = this.getFunctionType(node.initializer);
          }
        }

        return typeString;
      };

      const prepareReturnDefinitions = (definition: Definition, srcFile: SourceFileWithLocals) => {
        const [usedLocals, usedImports] = this.getUsedLocals(definition.type, srcFile);

        if (usedImports.length > 0) {
          usedImports.forEach((imp) => {
            definition.type = replaceImportIdentifier(definition.type, imp);
            definition.type = replaceDoubleImport(definition.type);
          });
        }

        const locals = [...usedLocals.values(), ...this.usedLocals.values()];

        return [
          definition,
          ...locals.flatMap((localSymbol) => {
            if (!this.visitedNodes.has(localSymbol)) {
              return this.getTypesDefinitions(localSymbol, moduleName);
            }

            return [];
          }),
        ];
      };

      if (symbol.name === KEYWORDS[ts.SyntaxKind.ExportAssignment] && realSymbol?.declarations) {
        const srcFile = getSourceFileFromNode(realSymbol.declarations[0]);
        const defaultDefinition: Definition = {
          moduleName,
          name: symbol.name,
          type: `any`,
          keyword: KEYWORDS[ts.SyntaxKind.ExportAssignment],
          syntaxKind: ts.SyntaxKind.ExportAssignment,
          locals: [...srcFile.locals.values()].reduce((acc: ts.Symbol[], symbol) => {
            const realSymbol = getRealSymbol(symbol, this.typeChecker);

            if (realSymbol) acc.push(realSymbol);

            return acc;
          }, []),
          filename: srcFile.fileName,
          symbol: realSymbol,
        };

        const aliasDeclaration = realSymbol.declarations[0];

        if (ts.isExportAssignment(initialNode)) {
          if (ts.isIdentifier(initialNode.expression)) {
            defaultDefinition.type = getTypeFromNodeExpression(aliasDeclaration, defaultDefinition);
          } else {
            defaultDefinition.type = getTypeFromNodeExpression(
              initialNode.expression,
              defaultDefinition,
            );
          }
        }

        if (ts.isExportSpecifier(initialNode)) {
          defaultDefinition.type = getTypeFromNodeExpression(aliasDeclaration, defaultDefinition);
        }

        return prepareReturnDefinitions(defaultDefinition, srcFile);
      }

      return (realSymbol.declarations ?? []).flatMap((node) => {
        const srcFile = getSourceFileFromNode(node);

        const definition: Definition = {
          moduleName,
          name: symbol.name,
          type: 'any',
          keyword: KEYWORDS[node.kind as KEYWORDS_KEYS],
          syntaxKind: node.kind,
          locals: [...srcFile.locals.values()].reduce((acc: ts.Symbol[], symbol) => {
            const realSymbol = getRealSymbol(symbol, this.typeChecker);

            if (realSymbol) acc.push(realSymbol);

            return acc;
          }, []),
          filename: srcFile.fileName,
          symbol: realSymbol,
        };

        definition.type = getTypeFromNodeExpression(node, definition);

        return prepareReturnDefinitions(definition, srcFile);
      });
    }

    return [];
  }
}
