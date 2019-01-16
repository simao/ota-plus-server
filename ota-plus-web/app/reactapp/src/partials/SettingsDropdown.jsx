/** @format */

import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import UserDropdown from './UserDropdown';
import { Avatar } from 'material-ui';
import { Dropdown, Button } from 'react-bootstrap';
import onClickOutside from 'react-onclickoutside';
import { LinkWrapper } from '../utils';
import $ from 'jquery';

@observer
class SettingsDropdown extends Component {
  render() {
    const { uiCredentialsDownload } = this.props;
    return (
      <Dropdown id='profile-dropdown' rootCloseEvent='mousedown'>
        <LinkWrapper bsRole='toggle'>
          {window.atsGarageTheme ? (
            <Avatar src='/assets/img/device_step_two.png' className='icon-profile' id='icon-profile-min' />
          ) : (
            <Avatar src='/assets/img/icons/Settings_Icon_small.svg' className='icon-profile' id='icon-profile-min' />
          )}
          &nbsp;
          <div className='dots nav-dots' id='settings-menu'>
            <span />
            <span />
            <span />
          </div>
        </LinkWrapper>
        <UserDropdown bsRole='menu' settings={true} uiCredentialsDownload={uiCredentialsDownload} />
      </Dropdown>
    );
  }
}

SettingsDropdown.propTypes = {};

export default onClickOutside(SettingsDropdown, {
  handleClickOutside: e => {
    return () => {
      if ($('#menu-login .dropdown').hasClass('open')) document.getElementById('profile-dropdown').click();
    };
  },
});
