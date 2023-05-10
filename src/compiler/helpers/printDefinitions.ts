import { Definition } from '../types';

const prepareDefinitionOutput = ({ keyword = 'const', type, name }: Definition) => {
  switch (keyword) {
    case 'class':
    case 'interface':
    case 'enum':
    case 'function':
    case 'type': {
      return type;
    }
    case 'default': {
      return `const _default: ${type}\n
                 export default _default`;
    }

    default: {
      return `${keyword} ${name}: ${type}`;
    }
  }
};

export const printDefinitions = (
  definitions: Definition[],
  dtsModuleMap: Record<string, Map<string, string>>,
  rootModule?: string,
) => {
  const output: string[] = [];
  const modulesOutput: Record<string, string[]> = {};

  definitions.forEach((definition) => {
    if (!modulesOutput[definition.moduleName]) modulesOutput[definition.moduleName] = [];

    const isDefault = definition.keyword === 'default';
    const result = prepareDefinitionOutput(definition);

    if (
      dtsModuleMap[definition.moduleName].has(definition.name) ||
      (dtsModuleMap[definition.moduleName] && isDefault)
    ) {
      modulesOutput[definition.moduleName].push(isDefault ? result : `export ${result}`);
    } else {
      output.push(result);
    }
  });

  const modules = Object.entries(modulesOutput).map(
    ([moduleName, moduleDefinitions]) =>
      `declare module "${
        rootModule ? `${rootModule}/` : ''
      }${moduleName}" {\n${moduleDefinitions.join('\n')}\n}\n`,
  );

  return [...output, ...modules].join('\n');
};
