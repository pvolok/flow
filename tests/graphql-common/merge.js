// @flow

// deep conflict
gql`
  fragment on User {
    friends {
      a: id
      friends { id }
    }
    friends {
      a: name
      friends(first: 1) { name }
    }
  }
`;

// overlap type and interface
gql`
  {
    union {
      ... on Story {
        a: text
        b: tags
      }
      ... on Event {
        b: id # ok: Story and Event don't overlap
      }
      ... on Node {
        a: id # error: Different fields.
      }
    }
  }
`;
