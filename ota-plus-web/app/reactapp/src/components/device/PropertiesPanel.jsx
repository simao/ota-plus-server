/** @format */

import React, { Component, PropTypes } from 'react';
import { observer, inject } from 'mobx-react';
import { observe, observable } from 'mobx';
import { FlatButton, DropDownMenu, MenuItem } from 'material-ui';
import { Loader } from '../../partials';
import { PropertiesList } from './properties';

const title = 'Properties';

@inject('stores')
@observer
class PropertiesPanel extends Component {
  render() {
    const { installPackage, packagesReady } = this.props;
    const { packagesStore } = this.props.stores;
    return (
      <div className='properties-panel'>
        <div className='properties-panel__header darkgrey-header'>{title}</div>
        <div className='properties-panel__wrapper'>
          {packagesStore.packagesFetchAsync.isFetching ? (
            <div className='wrapper-center'>
              <Loader />
            </div>
          ) : (
            <PropertiesList installPackage={installPackage} />
          )}
        </div>
      </div>
    );
  }
}

PropertiesPanel.propTypes = {
  installPackage: PropTypes.func.isRequired,
};

export default PropertiesPanel;
