import { createRegexpIdentifier } from '../createRegexpIdentifier';

describe('Test createRegexpIdentifier', () => {
  it('should be valid', () => {
    const identifier = 'Identifier';
    const regexp = createRegexpIdentifier(identifier);
    const validObject = `{
      name: ${identifier}
    }`;
    const validString = `const a = ${identifier}`;

    expect(validObject).toMatch(regexp);
    expect(validString).toMatch(regexp);
    expect(identifier).toMatch(regexp);
  });

  it('should be invalid', () => {
    const invalidIdentifier = 'InvalidIdentifier';
    const regexp = createRegexpIdentifier(invalidIdentifier);
    const invalidObject = `{
      name: '${invalidIdentifier}',
      ${invalidIdentifier}: '${invalidIdentifier}'
    }`;
    const invalidString = `const a = '${invalidIdentifier}'`;
    const importString = `import('moduleName').${invalidIdentifier}`;

    expect(invalidObject).not.toMatch(regexp);
    expect(invalidString).not.toMatch(regexp);
    expect(importString).not.toMatch(regexp);
  });
});
