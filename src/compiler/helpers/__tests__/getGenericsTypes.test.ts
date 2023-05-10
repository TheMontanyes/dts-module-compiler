import * as ts from 'typescript';
import { getGenericsTypes } from '../getGenericsTypes';

describe('Test getGenericsTypes', () => {
  it('should be valid', () => {
    const sourceFile = ts.createSourceFile(
      'ValidType',
      'type ValidType<T extends string = string, K extends Array<T> = Array<T>> = { array: K }',
      ts.ScriptTarget.ESNext,
      true,
    );

    const ValidType = sourceFile.statements[0] as ts.TypeAliasDeclaration;

    const genericsTypes = getGenericsTypes(ValidType.typeParameters!);

    expect(genericsTypes).toEqual('<T extends string = string, K extends Array<T> = Array<T>>');
  });
});
