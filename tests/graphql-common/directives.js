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
