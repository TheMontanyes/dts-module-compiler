import ts from 'typescript';

export const getGenericsTypes = (parameters: ts.NodeArray<ts.TypeParameterDeclaration>): string => {
  const generics = parameters.reduce((acc, parameter, index) => {
    if (parameter) {
      const value = parameter.getText();

      acc += parameters[index] !== parameters[parameters.length - 1] ? `${value}, ` : value;
    }

    return acc;
  }, '');

  return `<${generics}>`;
};
