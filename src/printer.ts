import {
  ClassDeclarationDefinition,
  FunctionDeclarationDefinition,
  ParsedModule,
  VariableDeclarationDefinition,
} from './types';
import ts from 'typescript';

export const printModule = (moduleName: string, parsedModule: ParsedModule) => {
  let moduleSource = ``;

  moduleSource += ts.ScriptElementKindModifier.ambientModifier;
  moduleSource += ' ';
  moduleSource += ts.ScriptElementKind.moduleElement;
  moduleSource += ' ';
  moduleSource += `"${moduleName}"`;
  moduleSource += ' ';
  moduleSource += '{';
  moduleSource += ts.sys.newLine;

  const reExportsIdentifiers: string[] = [];

  if (parsedModule.reExportsFromExternalModules.size > 0) {
    parsedModule.reExportsFromExternalModules.forEach((externalModule, packageName) => {
      const importItem = {
        identifiers: [] as string[],
        defaultName: '',
        packageName: '',
        namespace: '',
      };

      externalModule.forEach(
        ({
          identifierNameExport,
          isDefaultImport,
          isTypeOnlyExport,
          identifierNameImport,
          asNameExport,
          asNameImport,
          isNameSpaceImport,
        }) => {
          if (isTypeOnlyExport) {
            parsedModule.exportedIdentifiersTypeOnly.add(
              asNameExport ? `${identifierNameExport} as ${asNameExport}` : identifierNameExport,
            );
          } else {
            reExportsIdentifiers.push(
              asNameExport ? `${identifierNameExport} as ${asNameExport}` : identifierNameExport,
            );
          }

          if (isDefaultImport) {
            importItem.defaultName = identifierNameImport;
          }

          if (isNameSpaceImport) {
            importItem.namespace = identifierNameImport;
          }

          importItem.identifiers.push(
            asNameImport ? `${identifierNameImport} as ${asNameImport}` : identifierNameImport,
          );
        },
      );

      importItem.packageName = packageName;

      if (!importItem.defaultName && !importItem.identifiers.length) return;

      if (importItem.namespace) {
        moduleSource += `import * as ${importItem.namespace} from ${packageName}`;
        moduleSource += ts.sys.newLine;
        return;
      }

      let clauses = '';

      if (importItem.defaultName) {
        clauses += importItem.defaultName;

        if (importItem.identifiers.length > 0) {
          clauses += ',';
          clauses += ' ';
        }
      }

      if (importItem.identifiers.length > 0) {
        clauses += `{ ${importItem.identifiers.join(', ')} }`;
      }

      moduleSource += `import ${clauses} from ${packageName}`;
      moduleSource += ts.sys.newLine;
    });
  }

  if (parsedModule.linkedParsedNodes.size > 0) {
    parsedModule.linkedParsedNodes.forEach((linkedParsedNode) => {
      if (!linkedParsedNode.code) return;

      if (parsedModule.exportedParsedNodes.has(linkedParsedNode)) return;

      linkedParsedNode.code = linkedParsedNode.code.replace('export ', '');

      if (linkedParsedNode.jsDoc) {
        moduleSource += linkedParsedNode.jsDoc;
        moduleSource += ts.sys.newLine;
      }

      moduleSource += linkedParsedNode.code;
      moduleSource += ts.sys.newLine;
    });
  }

  if (parsedModule.exportedParsedNodes.size > 0) {
    parsedModule.exportedParsedNodes.forEach(({ code, jsDoc }) => {
      if (!code) return;

      if (jsDoc) {
        moduleSource += jsDoc;
        moduleSource += ts.sys.newLine;
      }

      moduleSource += code;
      moduleSource += ts.sys.newLine;
    });
  }

  if (parsedModule.exportedIdentifiersTypeOnly.size > 0) {
    moduleSource += ts.ScriptElementKindModifier.exportedModifier;
    moduleSource += ' ';
    moduleSource += ts.ScriptElementKind.typeElement;
    moduleSource += ' ';
    moduleSource += `{ ${[...parsedModule.exportedIdentifiersTypeOnly].join(', ')} }`;
    moduleSource += ts.sys.newLine;
  }

  if (reExportsIdentifiers.length > 0) {
    moduleSource += `export { ${reExportsIdentifiers.join(', ')} }`;
    moduleSource += ts.sys.newLine;
  }

  if (parsedModule.exportDefaultParsedNode) {
    moduleSource += parsedModule.exportDefaultParsedNode.code;
    moduleSource += ts.sys.newLine;
  }

  moduleSource += '}';

  return moduleSource;
};

export const printVariableDeclarationDefinition = ({
  identifierName,
  typeAnnotation,
  keyword,
  modifiers,
}: VariableDeclarationDefinition) => {
  let srcText = '';

  if (modifiers && modifiers.length > 0) {
    srcText += modifiers.join(' ');
    srcText += ' ';
  }

  srcText += keyword;
  srcText += ' ';
  srcText += identifierName;
  srcText += ':';
  srcText += ' ';
  srcText += typeAnnotation;

  return srcText;
};

export const printFunctionDeclarationDefinition = ({
  identifierName,
  parameters,
  returnType,
  keyword,
  modifiers,
  typeParameters,
}: FunctionDeclarationDefinition) => {
  let srcText = '';

  if (modifiers && modifiers.length > 0) {
    srcText += modifiers.join(' ');
    srcText += ' ';
  }

  srcText += keyword;
  srcText += ' ';
  srcText += identifierName;
  srcText += typeParameters;
  srcText += `(${parameters.join(', ')})`;
  srcText += ':';
  srcText += ' ';
  srcText += returnType;

  return srcText;
};

export const printClassDeclarationDefinition = ({
  identifierName,
  constructorParameters,
  members,
  heritageClauses,
  keyword,
  modifiers,
  typeParameters,
}: ClassDeclarationDefinition) => {
  let srcText = '';

  if (modifiers.length > 0) {
    srcText += modifiers.join(' ');
    srcText += ' ';
  }

  srcText += keyword;
  srcText += ' ';
  srcText += identifierName;
  srcText += typeParameters;
  srcText += ' ';

  if (heritageClauses.length > 0) {
    srcText += heritageClauses.join(' ');
    srcText += ' ';
  }

  srcText += '{';
  srcText += ts.sys.newLine;

  if (constructorParameters) {
    srcText += `constructor(${constructorParameters.join(', ')})`;
    srcText += ts.sys.newLine;
  }

  if (members.length > 0) {
    members.forEach((member) => {
      srcText += member;
      srcText += ts.sys.newLine;
    });
  }

  srcText += '}';

  return srcText;
};
