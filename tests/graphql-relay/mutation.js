// @flow

import {Mutation} from './relay';

class MyMutation extends Mutation {
  static fragments = {
    story: () => Relay.QL`fragment on Story {id}`,
  };

  getMutation() {
    return Relay.QL`mutation {likeStory(input: $input)}`; // `inc` not found in vars
  }

  getVariables() {
    this.props.story.id;
    this.props.story.aa; // not found
    return {storyID: 123}; // number ~> string
  }
}
