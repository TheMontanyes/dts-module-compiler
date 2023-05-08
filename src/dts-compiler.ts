import ts from 'typescript';
import path from 'node:path';

import { Definition } from './types';
import { getRealSymbol } from './helpers/getRealSymbol';
import { printDefinitions } from './helpers/printDefinitions';
import { fixDuplicatesDefinitions } from './helpers/fixDuplicatesDefinitions';
import { ConstructType } from './helpers/constructType';

async function compile({
  compilerOptions,
  exposeSrcToDestMap,
  rootModule,
  outFile = path.resolve(process.cwd(), 'dist', 'types.d.ts'),
}: {
  compilerOptions: ts.CompilerOptions;
  exposeSrcToDestMap: {
    [filename: string]: string;
  };
  rootModule?: string;
  outFile?: string;
}) {
  const fileNames = Object.keys(exposeSrcToDestMap);
  const program = ts.createProgram(fileNames, compilerOptions);
  const typeChecker = program.getTypeChecker();

  const stdLibTypes = new Map<string, ts.Symbol>();

  program.getSourceFiles().forEach((sourceFile) => {
    if (sourceFile.fileName.includes('lib.')) {
      typeChecker.getSymbolsInScope(sourceFile, ts.SymbolFlags.Type).forEach((symbol) => {
        if (symbol.declarations && symbol.declarations.length > 0) {
          const declaration = symbol.declarations[0];
          const type = typeChecker.getTypeAtLocation(declaration);
          const realSymbol = getRealSymbol(type.symbol, typeChecker);
          const symbolName = realSymbol?.getName();

          if (symbolName && !symbolName.includes('.') && symbolName !== '__type') {
            stdLibTypes.set(symbolName, realSymbol!);
          }
        }
      });
    }
  });

  const constructType = new ConstructType(program, typeChecker);
  const allDefinitions: Definition[] = [];
  const dtsModuleMap: Record<string, Map<string, string>> = {};

  fileNames.forEach((filename) => {
    const sourceFile = program.getSourceFile(filename);
    const moduleName = exposeSrcToDestMap[filename].replace(/\.\//, '');
    dtsModuleMap[moduleName] = new Map<string, string>();

    const symbolSourceFile = typeChecker.getSymbolAtLocation(sourceFile as ts.Node);

    if (symbolSourceFile) {
      const exportsOfModule = typeChecker.getExportsOfModule(symbolSourceFile);

      const moduleDefinitions = exportsOfModule.flatMap((exportSymbol) => {
        dtsModuleMap[moduleName].set(exportSymbol.name, exportSymbol.name);
        return constructType.getTypesDefinitions(exportSymbol, moduleName);
      });

      allDefinitions.push(...moduleDefinitions);
    }
  });

  const definitions = fixDuplicatesDefinitions(allDefinitions, dtsModuleMap, stdLibTypes);

  let resultDTS = printDefinitions(definitions, dtsModuleMap, rootModule);

  try {
    const prettier = require('prettier');
    const prettierConfig = await prettier.resolveConfig(process.cwd());

    resultDTS = prettier.format(resultDTS, { ...prettierConfig, parser: 'typescript' });
  } catch (error) {
    // ignore
  } finally {
    ts.sys.writeFile(outFile, resultDTS);
  }
}

export { compile };
