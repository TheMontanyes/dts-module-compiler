import ts from 'typescript';
import { isFromStdLib } from './helpers';

const getIdentifierFromQualifiedName = (
  node: ts.QualifiedName | ts.Identifier,
): ts.Identifier | null => {
  if (!ts.isQualifiedName(node) && !ts.isIdentifier(node)) return null;

  if (ts.isIdentifier(node)) return node;

  return getIdentifierFromQualifiedName(node.left);
};

export const createSearchLinkedNodesFromNode = (typeChecker: ts.TypeChecker) => {
  const cache = new Map<ts.Node, ts.Node[]>();

  function fromNode(node: ts.Node): ts.Node[] {
    if (isFromStdLib(node) || ts.isToken(node) || ts.isTypeParameterDeclaration(node)) {
      return [];
    }

    const collection = new Set<ts.Node>();

    const addToCollection = (node: ts.Node): void => {
      if (!isFromStdLib(node) && !ts.isTypeParameterDeclaration(node) && !ts.isToken(node)) {
        collection.add(node);
      }
    };

    if (ts.isTypeQueryNode(node)) {
      if (ts.isQualifiedName(node.exprName)) {
        const identifier = getIdentifierFromQualifiedName(node.exprName);

        if (identifier) {
          const symbolAtLocation = typeChecker.getSymbolAtLocation(identifier);

          const declaration = symbolAtLocation?.declarations?.[0];

          if (declaration) {
            addToCollection(declaration);
          }
        }
      } else {
        const symbolAtLocation = typeChecker.getSymbolAtLocation(node.exprName);

        const declaration = symbolAtLocation?.declarations?.[0];

        if (declaration) {
          addToCollection(declaration);
        }
      }
    }

    if (ts.isTypeOperatorNode(node)) {
      fromNode(node.type).forEach((node) => {
        addToCollection(node);
      });
    }

    if (ts.isTypeReferenceNode(node)) {
      const symbol = typeChecker.getSymbolAtLocation(node.typeName);
      const declarations = symbol?.declarations;

      node.typeArguments?.forEach((typeNode) => {
        fromNode(typeNode).forEach((node) => {
          addToCollection(node);
        });
      });

      if (declarations && declarations.length > 0) {
        declarations.forEach((declaration) => {
          addToCollection(declaration);
        });
      }
    }

    if (ts.isTypeLiteralNode(node)) {
      node.members.forEach((member) => {
        if (ts.isPropertySignature(member)) {
          if (member.type) {
            fromNode(member.type).forEach((node) => {
              addToCollection(node);
            });
          }
        }
      });
    }

    if (ts.isIntersectionTypeNode(node)) {
      node.types.forEach((typeNode) => {
        fromNode(typeNode).forEach((node) => {
          addToCollection(node);
        });
      });
    }

    if (ts.isFunctionLike(node)) {
      if (node.typeParameters?.length) {
        node.typeParameters.forEach((typeParameter) => {
          if (typeParameter.default) {
            fromNode(typeParameter.default).forEach((node) => {
              addToCollection(node);
            });
          }

          if (typeParameter.constraint) {
            fromNode(typeParameter.constraint).forEach((node) => {
              addToCollection(node);
            });
          }
        });
      }

      if (node.parameters.length > 0) {
        node.parameters.forEach((parameter) => {
          if (parameter.type) {
            const nodes = fromNode(parameter.type);

            if (nodes.length > 0) {
              nodes.forEach((node) => {
                addToCollection(node);
              });
            }
          }
        });
      }

      if (node.type) {
        const nodes = fromNode(node.type);

        if (nodes.length > 0) {
          nodes.forEach((node) => {
            addToCollection(node);
          });
        }
      }
    }

    if (ts.isUnionTypeNode(node)) {
      node.types.forEach((typeNode) => {
        const nodes = fromNode(typeNode);

        if (nodes.length > 0) {
          nodes.forEach((node) => {
            addToCollection(node);
          });
        }
      });
    }

    if (ts.isIndexedAccessTypeNode(node)) {
      fromNode(node.objectType).forEach((node) => {
        addToCollection(node);
      });
    }

    if (ts.isTemplateLiteralTypeNode(node)) {
      node.templateSpans.forEach((node) => {
        fromNode(node.type).forEach((node) => {
          addToCollection(node);
        });
      });
    }

    if (ts.isArrayTypeNode(node)) {
      fromNode(node.elementType).forEach((node) => {
        addToCollection(node);
      });
    }

    if (ts.isTupleTypeNode(node)) {
      node.elements.forEach((element) => {
        fromNode(element).forEach((node) => {
          addToCollection(node);
        });
      });
    }

    if (ts.isMappedTypeNode(node)) {
      const types = [node.type, node.nameType, node.typeParameter.constraint];

      types.forEach((type) => {
        if (type) {
          fromNode(type).forEach((node) => {
            addToCollection(node);
          });
        }
      });
    }

    if (ts.isConditionalTypeNode(node)) {
      [node.checkType, node.falseType, node.extendsType, node.trueType].forEach(
        (conditionalTypePart) => {
          fromNode(
            ts.isParenthesizedTypeNode(conditionalTypePart)
              ? conditionalTypePart.type
              : conditionalTypePart,
          ).forEach((node) => {
            addToCollection(node);
          });
        },
      );
    }

    const nodes = [...collection];
    cache.set(node, nodes);

    return nodes;
  }

  return fromNode;
};
