import fs from 'node:fs';

class TypesCache {
  static fsCache = new Map<string, string[]>();

  static getFsFiles(directory: string): string[] {
    if (this.fsCache.has(directory)) {
      return this.fsCache.get(directory)!;
    }

    const files = fs.readdirSync(directory);
    this.fsCache.set(directory, files);

    return files;
  }
}

export default TypesCache;
