import ts from 'typescript';
import { KEYWORDS } from './helpers/keywords';

export type SourceFileWithLocals = ts.SourceFile & {
  locals: Map<string, ts.Symbol>;
  classifiableNames: Set<string>;
};

export type Import = {
  name: string;
  from: string;
  asName?: string;
  isDefault: boolean;
  isExternal: boolean;
  isSVG?: boolean;
};

export type Definition = {
  name: string;
  type: string;
  keyword: KEYWORDS;
  syntaxKind: ts.SyntaxKind;
  filename: string;
  locals: ts.Symbol[];
  symbol: ts.Symbol;
  moduleName: string;
};
