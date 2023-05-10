import fs from 'node:fs';
import path from 'node:path';
import chalk from 'chalk';
import { Compiler, WebpackOptionsNormalized, WebpackPluginInstance } from 'webpack';

import { normalizeOptions } from './normalize-options';
import { download } from './download';
import TypescriptCompiler from './typescript-compiler';
import { ConfigOptions, MFOptions, NormalizeOptions } from './types';

class RemoteModuleTypesPlugin {
  normalizeOptions!: NormalizeOptions;
  webpackCompilerOptions!: WebpackOptionsNormalized;
  options: ConfigOptions = {};
  manifestRemotes: Record<string, string> = {};

  /**
   * @param {ConfigOptions} options
   * */
  constructor(options: ConfigOptions) {
    this.options = options;
  }

  /**
   * @param {import('webpack').Compiler} compiler
   * */
  apply(compiler: Compiler) {
    this.webpackCompilerOptions = compiler.options;

    compiler.hooks.emit.tap(RemoteModuleTypesPlugin.name, async () => {
      try {
        const MFPlugin = this.webpackCompilerOptions.plugins.find(
          (plugin) => plugin.constructor.name === 'ModuleFederationPlugin',
        ) as WebpackPluginInstance & { _options: MFOptions };

        if (MFPlugin) {
          this.normalizeOptions = normalizeOptions({
            tsDownloadsTypesFolder: this.options.tsDownloadsTypesFolder,
            tsBuildFolder: this.options.tsBuildFolder,
            tsConfigPath: this.options.tsConfigPath,
            mfName: MFPlugin._options.name ?? '',
            webpackCompilerOptions: this.webpackCompilerOptions,
          });

          if (this.webpackCompilerOptions.mode === 'production') {
            const hasExposes = Boolean((MFPlugin._options as MFOptions).exposes);

            if (hasExposes) {
              await this.compileTypes(
                (MFPlugin._options as MFOptions).exposes,
                this.options.additionalFiles,
              );
            }
          }

          if (this.webpackCompilerOptions.mode === 'development') {
            const { manifestRemotes } = this.options;

            if (typeof manifestRemotes === 'function') {
              this.manifestRemotes = await manifestRemotes();
            } else if (manifestRemotes) {
              this.manifestRemotes = manifestRemotes;
            }

            const hasRemotes = this.manifestRemotes && Object.keys(this.manifestRemotes).length > 0;

            if (hasRemotes) {
              await this.downloadTypes(
                Object.keys(this.manifestRemotes),
                this.normalizeOptions.tsDownloadsTypesFolder,
              );
            }
          }
        }
      } catch (error) {
        console.log(chalk.red(error));
      }
    });
  }

  async downloadTypes(remotes: string[], destTypesFolder: string) {
    console.log(
      chalk.blue(
        `${RemoteModuleTypesPlugin.name}: [INFO] Start downloading remote typescript files`,
      ),
    );

    for (let i = 0; i < remotes.length; i++) {
      const remoteName = remotes[i];
      const dirDest = path.join(destTypesFolder, remoteName);
      const filename = `${remoteName}.d.ts`;

      if (!fs.existsSync(dirDest)) {
        fs.mkdirSync(dirDest, { recursive: true });
      }

      try {
        const url = new URL(this.manifestRemotes[remoteName]);
        const pathname = `${url.pathname}/${filename}`.replace(/\/{2}/g, '/');

        await download({
          filename,
          dirDest,
          requestOptions: {
            ...(this.options?.requestOptions ?? {}),
            method: 'GET',
            host: url.host,
            path: pathname,
          },
        });
      } catch (error) {
        console.error(
          chalk.red(
            `${RemoteModuleTypesPlugin.name}: [ERROR] an error occurred while downloading ${remoteName}\nReason: ${error}`,
          ),
        );
      }
    }
    console.log(
      chalk.blue(
        `${RemoteModuleTypesPlugin.name}: [INFO] Download of remote typescript files completed`,
      ),
    );
  }

  async compileTypes(exposedFiles: MFOptions['exposes'], additionalFiles?: string[]) {
    const compiler = new TypescriptCompiler(this.normalizeOptions);

    try {
      await compiler.generateDeclarationFiles(exposedFiles, additionalFiles);
    } catch (error) {
      console.log(error);
      throw new Error();
    }
  }
}

export { RemoteModuleTypesPlugin };
