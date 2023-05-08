import * as ts from 'typescript';
import { getMockSourceFile } from './utils/getMockSourceFile';
import { getMemberType } from '../getMemberType';

describe('Test getMemberType', () => {
  it('should be valid', () => {
    const sourceFile = getMockSourceFile();
    const members: string[] = [];

    sourceFile.statements.forEach((statement) => {
      if (ts.isTypeAliasDeclaration(statement)) {
        statement.forEachChild((child) => {
          if (ts.isTypeLiteralNode(child)) {
            child.members.forEach((member) => {
              const memberType = getMemberType(member);
              members.push(memberType.replace(/\n/, ''));
            });
          }
        });
      }
    });

    expect(members).toEqual([
      'array: K',
      'array: string[];',
      'name: string;',
      'id: number;',
      'json: { id: string; name: string }',
      '[Key]: any;',
    ]);
  });
});
