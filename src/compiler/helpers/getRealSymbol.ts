// eslint-disable-next-line no-bitwise
import ts from 'typescript';
import { SourceFileWithLocals } from '../types';

const isCanBeAliasSymbol = (symbol: ts.Symbol) => symbol.flags & ts.SymbolFlags.Alias;

export const getRealSymbol = (symbol: ts.Symbol | undefined, typeChecker: ts.TypeChecker) => {
  if (!symbol) return;

  return isCanBeAliasSymbol(symbol) ? typeChecker.getAliasedSymbol(symbol) : symbol;
};
