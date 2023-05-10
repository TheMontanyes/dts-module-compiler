export const getPathsToSrcFilesFromType = (typeString: string) => {
  const regexp = /import\(['"](.*?)['"]\)/g;
  const matched = typeString.match(regexp);

  if (!matched) return [];

  return matched
    .filter((path) => !/[\\/]node_modules[\\/]/.test(path))
    .map((path) => path.replace(regexp, '$1') + '.ts');
};
