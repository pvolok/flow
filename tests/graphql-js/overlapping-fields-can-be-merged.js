// @flow

// unique fields
gql`
  fragment uniqueFields on Dog {
    name
    nickname
  }
`;

// identical fields
gql`
  fragment mergeIdenticalFields on Dog {
    name
    name
  }
`;

// identical fields with identical args
gql`
  fragment mergeIdenticalFieldsWithIdenticalArgs on Dog {
    doesKnowCommand(dogCommand: SIT)
    doesKnowCommand(dogCommand: SIT)
  }
`;

// identical fields with identical directives
gql`
  fragment mergeSameFieldsWithSameDirectives on Dog {
    name @include(if: true)
    name @include(if: true)
  }
`;

// different args with different aliases
gql`
  fragment differentArgsWithDifferentAliases on Dog {
    knowsSit: doesKnowCommand(dogCommand: SIT)
    knowsDown: doesKnowCommand(dogCommand: DOWN)
  }
`;

// different directives with different aliases
gql`
  fragment differentDirectivesWithDifferentAliases on Dog {
    nameIfTrue: name @include(if: true)
    nameIfFalse: name @include(if: false)
  }
`;

// different skip/include directives accepted
gql`
  fragment differentDirectivesWithDifferentAliases on Dog {
    name @include(if: true)
    name @include(if: false)
  }
`;

// Same aliases with different field targets
gql`
  fragment sameAliasesWithDifferentFieldTargets on Dog {
    fido: name
    fido: nickname # error: Different fields.
  }
`;

// Same aliases allowed on non-overlapping fields
// This is valid since no object can be both a "Dog" and a "Cat", thus
// these fields can never overlap.
gql`
  fragment sameAliasesWithDifferentFieldTargets on Pet {
    ... on Dog {
      name
    }
    ... on Cat {
      name: nickname
    }
  }
`;

// Alias masking direct field access
gql`
  fragment aliasMaskingDirectFieldAccess on Dog {
    name: nickname
    name # error: Different fields.
  }
`;

// different args, second adds an argument
gql`
  fragment conflictingArgs on Dog {
    doesKnowCommand
    doesKnowCommand(dogCommand: HEEL) # error: Different arguments.
  }
`;

// different args, second missing an argument
gql`
  fragment conflictingArgs on Dog {
    doesKnowCommand(dogCommand: SIT)
    doesKnowCommand # error: Different arguments.
  }
`;

// conflicting args
gql`
  fragment conflictingArgs on Dog {
    doesKnowCommand(dogCommand: SIT)
    doesKnowCommand(dogCommand: HEEL) # error: Different arguments.
  }
`;

// allows different args where no conflict is possible
// This is valid since no object can be both a "Dog" and a "Cat", thus
// these fields can never overlap.
gql`
  fragment conflictingArgs on Pet {
    ... on Dog {
      name(surname: true)
    }
    ... on Cat {
      name
    }
  }
`;
