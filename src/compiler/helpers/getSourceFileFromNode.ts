import ts from 'typescript';
import { SourceFileWithLocals } from '../types';

export const getSourceFileFromNode = (node: ts.Node) => {
  return node.getSourceFile() as SourceFileWithLocals;
};
