define(function(require) {
  var React = require('react'),
      SotaDispatcher = require('sota-dispatcher'),
      Router = require('react-router'),
      Link = Router.Link;

  class QueueListItem extends React.Component {
    constructor(props) {
      super(props);
      this.cancelUpdate = this.cancelUpdate.bind(this);
    }
    cancelUpdate() {
      SotaDispatcher.dispatch({
        actionType: 'cancel-update',
        device: this.props.deviceId,
        updateid: this.props.package.requestId
      });
    }
    render() {
      var packageName = this.props.package.packageId.name;
      packageName = packageName.length > 30 ? packageName.substring(0, 30) + '..' : packageName;
      return (
        <li className={'list-group-item ' + this.props.status + ' queue-item-status-' + this.props.package.status}>
          <span className="list-group-item-text-left">{packageName}</span>
          {!_.isUndefined(this.props.package.status) && this.props.package.status == 'InFlight' ? 
            <div className="queue-inflight-box pull-right">
              in progress
            </div>
          :
            <span>
              <span className="drag-bar pull-right"><i className="fa fa-bars"></i></span>
              <button className="btn btn-action pull-right" onClick={this.cancelUpdate} id="button-cancel-update">{this.context.strings.cancel}</button>
              {this.props.status == 'error' ?
                <button className="btn btn-action pull-right">retry</button>
              : null}
            </span>
          }
          
          <div className="pull-right list-group-item-text-right">
            {this.props.status == 'error' ?
              <span className="fa-stack package-status-icon">
                <i className="fa fa-times-circle fa-stack-1x red" aria-hidden="true"></i>
              </span>
            : null}
            <strong>v. {this.props.package.packageId.version}</strong>
          </div>
        </li>
      );
    }
  };

  QueueListItem.contextTypes = {
    strings: React.PropTypes.object.isRequired,
  };

  return QueueListItem;
});
