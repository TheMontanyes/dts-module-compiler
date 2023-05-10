import path from 'node:path';
import fs from 'node:fs';
import ts from 'typescript';
import merge from 'lodash.merge';
import { WebpackOptionsNormalized } from 'webpack';

const { ModuleResolutionKind, ModuleKind, ScriptTarget, JsxEmit } = ts;

const TYPESCRIPT_FOLDER_DEFAULT_NAME = path.join(process.cwd(), '@mf-types');

const getTSConfig = (pathToTSConfig: string) => {
  try {
    const existPath = pathToTSConfig;
    let config = JSON.parse(fs.readFileSync(existPath).toString('utf8'));

    if (config.extends) {
      const extendedConfig = JSON.parse(
        fs.readFileSync(path.join(path.dirname(existPath), config.extends)).toString('utf8'),
      );

      config = merge(config, extendedConfig);
    }

    return config;
  } catch (e) {
    new Error(
      'RemoteModuleTypesPlugin: [ERROR] Error reading tsconfig.json. Check the specified path or the validity of the file.',
    );
  }
};

const normalizeOptions = ({
  webpackCompilerOptions,
  mfName,
  tsDownloadsTypesFolder = TYPESCRIPT_FOLDER_DEFAULT_NAME,
  tsBuildFolder = webpackCompilerOptions.output.path,
  tsConfigPath = path.join(process.cwd(), 'tsconfig.json'),
}: {
  tsDownloadsTypesFolder?: string;
  tsBuildFolder?: string;
  tsConfigPath?: string;
  mfName: string;
  webpackCompilerOptions: WebpackOptionsNormalized;
}) => {
  const tsCompilerOptions = {
    ...getTSConfig(tsConfigPath).compilerOptions,
    moduleResolution: ModuleResolutionKind.NodeJs,
    module: ModuleKind.CommonJS,
    target: ScriptTarget.ES2019,
    declaration: true,
    emitDeclarationOnly: true,
    esModuleInterop: true,
    jsx: JsxEmit.ReactJSX,
    outFile: path.join(tsBuildFolder!, `${mfName}.d.ts`),
  };

  return {
    tsCompilerOptions,
    tsDownloadsTypesFolder,
    webpackCompilerOptions,
    mfName,
  };
};

export { normalizeOptions };
