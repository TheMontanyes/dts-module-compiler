import ts from 'typescript';

export const isImportExternalModule = (importPath: string, options: ts.CompilerOptions) => {
  const aliasPaths = Object.keys(options.paths ?? {});

  for (let i = 0; i < aliasPaths.length; i++) {
    const path = aliasPaths[i];

    if (importPath.match(path)) {
      return false;
    }
  }

  return !importPath.match(/\.\//) && !importPath.match(/^['"]\.["']$/);
};
