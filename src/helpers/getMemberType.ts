import ts from 'typescript';

export const getMemberType = (member: ts.TypeElement) => {
  const memberName = member.name?.getText();

  let result = '';
  let expressionMember = '';

  if (memberName) {
    if (memberName.startsWith('[') && memberName.endsWith(']')) {
      const replacedMemberName = memberName;
      const regexp = /\[(import\(.*\).*?)]/;

      result += `\n${
        regexp.test(replacedMemberName)
          ? replacedMemberName.replace(regexp, '[key in typeof $1]')
          : replacedMemberName
      }`;
    } else {
      result += `\n${memberName}`;
    }
    expressionMember = member.getText().replace(memberName, '');
  } else {
    expressionMember = member.getText();
  }

  result += expressionMember;

  return result;
};
