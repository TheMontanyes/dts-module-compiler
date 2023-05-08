import * as ts from 'typescript';
import fs from 'node:fs';
import path from 'node:path';

export const getMockSourceFile = () =>
  ts.createSourceFile(
    'ValidType',
    fs.readFileSync(path.resolve(__dirname, 'mockFile.ts')).toString('utf-8'),
    ts.ScriptTarget.ESNext,
    true,
  );
