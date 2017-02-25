// @flow

const React = require('react');
const Relay = require('react-relay');

class Statefull extends React.Component {
  render() {
    this.props.user.id;
    this.props.user.kappa;
    this.props.relay;
  }
}

const StatefullR = Relay.createContainer(Statefull, {
  fragments: {
    user: () => Relay.QL`
      fragment on Story {
        id
        tags
      }
    `,
  }
});

<StatefullR />;
<StatefullR user={123} />;
