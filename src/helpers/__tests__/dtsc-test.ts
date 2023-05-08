import * as ts from 'typescript';
import * as path from 'node:path';
import { compile } from '../../dts-compiler';
import tsConfig from '../../../tsconfig.json';

const compilerOptions = tsConfig.compilerOptions as unknown as ts.CompilerOptions;

compile({
  compilerOptions: {
    ...compilerOptions,
    module: ts.ModuleKind.CommonJS,
    moduleResolution: ts.ModuleResolutionKind.NodeNext,
    noEmit: true,
    declaration: true,
    esModuleInterop: true,
    emitDeclarationOnly: true,
  },
  exposeSrcToDestMap: {
    [path.resolve(__dirname, './mockFile.ts')]: './test',
  },
});
