import { Definition } from '../types';
import ts from 'typescript';
import { getSourceFileFromNode } from './getSourceFileFromNode';
import { createRegexpIdentifier } from './createRegexpIdentifier';

export const fixDuplicatesDefinitions = (
  definitions: Definition[],
  dtsModuleMap: Record<string, Map<string, string>>,
  stdLibTypes: Map<string, ts.Symbol>,
) => {
  const result = definitions.filter((definition, index, array) => {
    return (
      index ===
      array.findIndex(
        (someDefinition) =>
          definition.name === someDefinition.name &&
          definition.syntaxKind === someDefinition.syntaxKind &&
          definition.filename === someDefinition.filename,
      )
    );
  });

  const duplicatesCount: Record<string, number> = {};

  const duplicatesNameDefinitions = result.reduce((acc, definition, index, self) => {
    const isDuplicate =
      (self.some((someDefinition) => {
        return (
          someDefinition.name === definition.name && someDefinition.filename !== definition.filename
        );
      }) ||
        stdLibTypes.has(definition.name)) &&
      !dtsModuleMap[definition.moduleName].has(definition.name);

    if (isDuplicate) {
      const duplicateKey = `${definition.name}${definition.filename}${definition.syntaxKind}`;
      const duplicateCountKey = definition.name;

      duplicatesCount[duplicateCountKey] = duplicatesCount[duplicateCountKey]
        ? duplicatesCount[duplicateCountKey] + 1
        : 1;

      acc[duplicateKey] = duplicatesCount[duplicateCountKey];

      const maybeDuplicateTypeAlias =
        acc[`${definition.name}${definition.filename}${ts.SyntaxKind.TypeAliasDeclaration}`];
      const maybeDuplicateVar =
        acc[`${definition.name}${definition.filename}${ts.SyntaxKind.VariableDeclaration}`];

      if (maybeDuplicateTypeAlias && maybeDuplicateVar) {
        acc[duplicateKey] = Math.min(maybeDuplicateTypeAlias, maybeDuplicateVar);
        duplicatesCount[duplicateCountKey] = Math.min(maybeDuplicateTypeAlias, maybeDuplicateVar);
      }

      if (duplicatesCount[duplicateCountKey]) {
        const newName = `${definition.name}_${duplicatesCount[duplicateCountKey]}`;
        definition.type = definition.type.replace(
          new RegExp(`${definition.keyword} ${definition.name}`),
          `${definition.keyword} ${newName}`,
        );
        definition.name = newName;
      }
    }

    return acc;
  }, {} as Record<string, number>);

  result.forEach((definition) => {
    const name = definition.name.split('_')[0];

    definition.locals.forEach((definitionLocaleSymbol) => {
      if ((definitionLocaleSymbol.declarations?.length ?? 0) > 0) {
        definitionLocaleSymbol.declarations!.forEach((node) => {
          if (definitionLocaleSymbol.name === name && definition.syntaxKind === node.kind) return;

          if (node) {
            const srcFile = getSourceFileFromNode(node);
            const duplicateKey = `${definitionLocaleSymbol.name}${srcFile.fileName}${node.kind}`;

            if (duplicatesNameDefinitions[duplicateKey]) {
              definition.type = definition.type.replace(
                createRegexpIdentifier(definitionLocaleSymbol.name),
                `${definitionLocaleSymbol.name}_${duplicatesNameDefinitions[duplicateKey]}`,
              );
            }
          }
        });
      }
    });
  });

  return result;
};
