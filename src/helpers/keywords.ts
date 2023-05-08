import ts from 'typescript';

export const KEYWORDS = {
  [ts.SyntaxKind.VariableDeclaration]: 'const',
  [ts.SyntaxKind.ArrowFunction]: 'const',
  [ts.SyntaxKind.FunctionDeclaration]: 'function',
  [ts.SyntaxKind.TypeAliasDeclaration]: 'type',
  [ts.SyntaxKind.ClassDeclaration]: 'class',
  [ts.SyntaxKind.EnumDeclaration]: 'enum',
  [ts.SyntaxKind.InterfaceDeclaration]: 'interface',
  [ts.SyntaxKind.ExportAssignment]: 'default',
} as const;

export type KEYWORDS_KEYS = keyof typeof KEYWORDS;
export type KEYWORDS = (typeof KEYWORDS)[KEYWORDS_KEYS];
