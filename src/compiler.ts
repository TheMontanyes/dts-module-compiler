import path from 'node:path';
import ts from 'typescript';
import { createSourceFileParser } from './parser';
import { CompileOptions } from './types';
import { printModule } from './printer';
import { getCompilerOptions } from './helpers';

// TODO Search refactoring
// TODO Parser refactoring
// TODO Prettier
// TODO Options API

const baseOutputPath = path.resolve(process.cwd(), 'build', 'types.ts');
const baseTsConfigPath = path.resolve(process.cwd(), 'tsconfig.json');

export default function main({
  moduleList,
  outputPath = baseOutputPath,
  tsconfig = baseTsConfigPath,
}: CompileOptions) {
  console.time('compile');
  const compilerOptions = getCompilerOptions(tsconfig);
  const exposeEntries = Object.entries(moduleList);

  const program = ts.createProgram(
    exposeEntries.map(([_, fileName]) => fileName),
    compilerOptions,
  );

  const typeChecker = program.getTypeChecker();

  const sourceFileParser = createSourceFileParser(typeChecker);

  const resultSourceCodeDTS = exposeEntries.reduce((acc, [moduleName, fileName]) => {
    const sourceFile = program.getSourceFile(fileName);

    if (sourceFile) {
      const parsedFile = sourceFileParser(sourceFile);

      acc += `${printModule(moduleName, parsedFile)}${ts.sys.newLine}`;
    } else {
      throw new Error(`Not found file at - ${fileName}`);
    }

    return acc;
  }, '');

  if (resultSourceCodeDTS) {
    ts.sys.writeFile(outputPath, resultSourceCodeDTS);
  }

  console.timeEnd('compile');
}
