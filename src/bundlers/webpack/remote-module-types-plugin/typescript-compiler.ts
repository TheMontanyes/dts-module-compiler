import path from 'node:path';
import fs from 'node:fs';
import TypesCache from './types-cache';
import rmrf from 'rmrf';
import { MFOptions, NormalizeOptions } from './types';
import { compile } from '../../../compiler';

class TypescriptCompiler {
  constructor(private options: NormalizeOptions) {
    this.options = options;
  }

  async generateDeclarationFiles(
    exposedFiles: MFOptions['exposes'],
    additionalFiles: string[] = [],
  ) {
    if (!exposedFiles) return;

    const exposeSrcToDestMap = Object.entries(exposedFiles).reduce(
      (acc, [exposeDest, exposeSrc]) => {
        const pathWithExt = this.getNormalizedPathWithExt(exposeSrc);

        if (/\.tsx?$/.test(pathWithExt)) {
          acc[pathWithExt] = exposeDest;
        }

        return acc;
      },
      {} as Record<string, string>,
    );

    const buildDir = path.dirname(this.options.tsCompilerOptions.outFile);

    if (fs.existsSync(buildDir)) {
      rmrf(buildDir);
    }

    await compile({
      compilerOptions: this.options.tsCompilerOptions,
      exposeSrcToDestMap,
      rootModule: this.options.mfName,
      outFile: this.options.tsCompilerOptions.outFile,
    });

    const additionalFilesSrc = additionalFiles.reduce((acc: string[], file: string) => {
      if (file.endsWith('.d.ts')) {
        acc.push(fs.readFileSync(file).toString('utf8'));
      }

      return acc;
    }, []);

    if (additionalFilesSrc.length > 0) {
      fs.writeFileSync(
        this.options.tsCompilerOptions.outFile,
        `${additionalFilesSrc.join('\n')}\n${fs.readFileSync(
          this.options.tsCompilerOptions.outFile,
        )}`,
      );
    }
  }

  getNormalizedPathWithExt(exposeSrc: string) {
    const cwd = process.cwd();

    const [rootDir, entry] = exposeSrc.split(/\/(?=[^/]+$)/);

    const normalizedRootDir = path.resolve(cwd, rootDir);
    const filenameWithExt = this.getFilenameWithExtension(normalizedRootDir, entry);

    return path.resolve(normalizedRootDir, filenameWithExt);
  }

  getFilenameWithExtension(rootDir: string, entry: string) {
    if (!fs.existsSync(rootDir) || !fs.lstatSync(rootDir).isDirectory()) {
      throw new Error('Аргумент rootDir должен быть директорией');
    }

    let filename;

    try {
      const files = TypesCache.getFsFiles(path.join(rootDir, entry));

      filename = files?.find((file: string) => file.split('.')[0] === 'index');

      if (!filename) {
        throw new Error(`RemoteModuleTypesPlugin: [ERROR] File ${filename} not found`);
      }

      return `${entry}/${filename}`;
    } catch (err) {
      const files = TypesCache.getFsFiles(rootDir);

      filename = files?.find((file: string) => {
        const baseFile = path.basename(file, path.extname(file));
        const baseEntry = path.basename(entry, path.extname(entry));

        return baseFile === baseEntry;
      });

      if (!filename) {
        throw new Error(`RemoteModuleTypesPlugin: [ERROR] File ${filename} not found`);
      }

      return filename;
    }
  }
}

export default TypescriptCompiler;
