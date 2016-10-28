define(function(require) {
  var React = require('react'),
      Router = require('react-router'),
      Link = Router.Link,
      SearchBar = require('es6!../searchbar'),
      PackagesList = require('es6!./packages-list');

  class Packages extends React.Component {
    constructor(props, context) {
      super(props, context);
      this.state = {
        filterValue: '',
        selectedSort: 'asc',
        selectedSortName: 'A > Z',
        packagesListHeight: '300px',
        isFormShown: false
      }
      this.changeFilter = this.changeFilter.bind(this);
      this.openForm = this.openForm.bind(this);
      this.closeForm = this.closeForm.bind(this);
      this.setPackagesListHeight = this.setPackagesListHeight.bind(this);
    }
    componentDidMount() {
      window.addEventListener("resize", this.setPackagesListHeight);
      this.setPackagesListHeight();
    }
    componentWillUnmount() {
      window.removeEventListener("resize", this.setPackagesListHeight);
    }
    openForm() {
      this.setState({
        isFormShown: true
      });
    }
    closeForm() {
      this.setState({
        isFormShown: false
      });
    }
    setPackagesListHeight() {
      var windowHeight = jQuery(window).height();
      var offsetTop = jQuery('#packages-wrapper').offset().top;
            
      this.setState({
        packagesListHeight: windowHeight - offsetTop
      });
    }
    changeFilter(filter) {
      this.setState({filterValue: filter});
    }
    selectSort(sort, e) {
      e.preventDefault();

      var name = jQuery(e.target).text();
      this.setState({
        selectedSort: sort,
        selectedSortName: name
      });
    }
    render() {
      return (
        <div>
          <div className="panel panel-ats">
            <div className="panel-body">
              <div className="panel-subheading">
                <div className="container">
                  <SearchBar class="search-bar pull-left" inputId="search-packages-input" changeFilter={this.changeFilter}/>
                  
                  <div className="select-bar pull-left margin-left-100">
                    <div className="select-bar-text">Sort by</div>
                    <div className="btn-group">
                      <button type="button" className="btn btn-grey dropdown-toggle" data-toggle="dropdown" aria-haspopup="true" aria-expanded="false">
                        <span className="pull-left">{this.state.selectedSortName} &nbsp;</span>
                        <span className="fa fa-angle-down pull-right"></span>
                      </button>
                      <ul className="dropdown-menu">
                        <li><a href="#" onClick={this.selectSort.bind(this, 'asc')} id="link-sort-packages-asc">A &gt; Z</a></li>
                        <li><a href="#" onClick={this.selectSort.bind(this, 'desc')} id="link-sort-packages-desc">Z &gt; A</a></li>
                      </ul>
                    </div>
                  </div>
          
                  <button onClick={this.openForm} className="btn btn-main btn-add pull-right" id="button-add-new-package">
                    <i className="fa fa-plus"></i> &nbsp; Add new package
                  </button>
                </div>
              </div>
              <div id="packages-wrapper" style={{height: this.state.packagesListHeight}}>
                <PackagesList 
                  websocket={this.props.websocket}
                  packagesListHeight={this.state.packagesListHeight}
                  selectedSort={this.state.selectedSort}
                  filterValue={this.state.filterValue}
                  isFormShown={this.state.isFormShown}
                  openForm={this.openForm}
                  closeForm={this.closeForm}/>
              </div>
            </div>
          </div>
        </div>
      );
    }
  };

  Packages.contextTypes = {
    strings: React.PropTypes.object.isRequired
  };

  return Packages;
});
