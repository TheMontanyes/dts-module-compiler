import { Import } from '../types';
import { replaceImport } from './replaceImport';
import { createRegexpIdentifier } from './createRegexpIdentifier';

export const replaceImportIdentifier = (code: string, usedImport: Import) => {
  let text = code.includes('src') || code.includes('node_modules') ? replaceImport(code) : code;

  const { name, asName, isDefault, from, isExternal } = usedImport;

  const regexp = createRegexpIdentifier(asName || name);

  if (!regexp.test(text)) return text;

  if (isExternal) {
    if (isDefault) {
      text = text.replace(regexp, `import(${from})`);
    } else {
      text = text.replace(regexp, `import(${from}).${name}`);
    }
  }

  return text;
};
