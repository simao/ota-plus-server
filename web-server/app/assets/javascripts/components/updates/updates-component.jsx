define(function(require) {

  var React = require('react'),
      Router = require('react-router'),
      Fluxbone = require('../../mixins/fluxbone'),
      SotaDispatcher = require('sota-dispatcher');

  var Updates = React.createClass({
    mixins: [
      Fluxbone.Mixin("Store", "sync")
    ],
    componentDidMount: function(props, context) {
      this.props.Store.fetch();
    },
    render: function() {
      var rows = this.props.Store.models.map(function(update) {
        var startend = update.get('periodOfValidity').split("/");
        return (
          <tr>
            <td>
              {update.get('packageId').name}
            </td>
            <td>
              {update.get('packageId').version}
            </td>
            <td>
              {startend[0]}
            </td>
            <td>
              {startend[1]}
            </td>
            <td>
              {update.get('priority')}
            </td>
            <td>
              <Router.Link to='update' params={{id: update.get('id'), Model: update}}>
                Details
              </Router.Link>
            </td>
          </tr>
        );
      });

      return (
        <div>
          <div className="row">
            <div className="col-md-12">
              <h1>
                Updates
              </h1>
            </div>
          </div>
          <div className="row">
            <div className="col-md-8">
              <p>
              </p>
            </div>
          </div>
          <table className="table table-striped table-bordered">
            <thead>
              <tr>
                <td>
                  Package
                </td>
                <td>
                  Version
                </td>
                <td>
                  Start
                </td>
                <td>
                  End
                </td>
                <td>
                  Priority
                </td>
                <td>
                </td>
              </tr>
            </thead>
            <tbody>
              { rows }
            </tbody>
          </table>
        </div>
      );
    }
  });

  return Updates;
});
