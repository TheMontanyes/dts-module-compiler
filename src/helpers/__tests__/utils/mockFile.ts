type ValidType<K extends [...any]> = { array: K };
type ValidType2 = { array: string[]; name: string; id: number; json: { id: string; name: string } };

const Key = 'Key' as const;

type DynamicKey = {
  [Key]: any;
};

type MappedWithImports = {
  [key in typeof import('typescript/lib/typescript').SyntaxKind.TypeAliasDeclaration]: any;
};

const shortland = 'shortland';

const MemeberShortland = {
  shortland,
} as const;

type MemeberShortland = typeof MemeberShortland;

type ExternalType = typeof import('../../getGenericsTypes').getGenericsTypes;
