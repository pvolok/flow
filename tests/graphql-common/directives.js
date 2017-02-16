// @flow

declare function query<T>(T): $GraphqlData<T>;

const stories = query(gql`
  fragment on Story @relay(plural: true) {
    maybe: id @include(if: false)
    merged: id
    ... on Story @skip(if: true) {
      merged: id
      inlineFragSkipped: id
    }
  }
`);

(stories: Array<{|
  maybe: ?string,
  merged: string,
  inlineFragSkipped: ?string,
|}>);

gql`
  query @on_query {
    version @on_field
    ... on Query @on_inline_fragment { __typename }
  }
`;
gql`mutation @on_mutation { likeStory(storyID: "1", inc: 1) }`;
gql`
  fragment on Story @on_fragment_definition {
    id
  }
`;

gql`query @on_field { version }`; // error: FIELD directory not allowed here
