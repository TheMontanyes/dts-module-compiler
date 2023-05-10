import ts from 'typescript';

export const isCustomType = (symbol: ts.Symbol, program: ts.Program): boolean => {
  if (!symbol) return false;

  const sourceFile = (symbol.declarations ?? [])[0].getSourceFile();

  return !sourceFile || !program.isSourceFileDefaultLibrary(sourceFile);
};
