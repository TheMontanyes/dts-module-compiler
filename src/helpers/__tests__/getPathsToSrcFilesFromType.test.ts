import * as ts from 'typescript';
import { getMockSourceFile } from './utils/getMockSourceFile';
import { getPathsToSrcFilesFromType } from '../getPathsToSrcFilesFromType';

describe('Test getPathsToSrcFilesFromType', () => {
  it('should be valid', () => {
    const sourceFile = getMockSourceFile();

    const ExternalType = sourceFile.statements.find((statement) =>
      statement.getText().includes('ExternalType'),
    ) as ts.TypeAliasDeclaration;

    expect(getPathsToSrcFilesFromType(ExternalType.type.getText())).toEqual([
      '../../getGenericsTypes.ts',
    ]);
  });
});
