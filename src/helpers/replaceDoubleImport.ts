export const replaceDoubleImport = (text: string) =>
  text.replace(/import\('(import\('.*'\))'\)/gm, '$1');
