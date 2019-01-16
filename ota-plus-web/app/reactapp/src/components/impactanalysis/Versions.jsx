/** @format */

import PropTypes from 'prop-types';
import React, { Component } from 'react';
import { observer } from 'mobx-react';
import _ from 'lodash';
import VersionsItem from './VersionsItem';

@observer
class Versions extends Component {
  render() {
    const { versions } = this.props;
    return (
      <div className='versions' id='impact-analysis-blacklisted-versions'>
        <ul>
          {_.map(versions, (version, index) => {
            return <VersionsItem version={version} key={index} />;
          })}
        </ul>
      </div>
    );
  }
}

Versions.propTypes = {
  // versions: PropTypes.object.isRequired,
};

export default Versions;
