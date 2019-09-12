import styled from 'styled-components';
import React, { ReactElement } from 'react';
import { NavLink } from 'react-router-dom';
import { useTranslation, UseTranslationResponse } from 'react-i18next';
import { COLORS } from '../../constants/styleConstants';

const Navbar = styled.nav`
  height: 50px;
  background-color: ${({ theme }) => theme.palette.secondary};
  display: flex;
  align-items: center;
  position: sticky;
  top: 0;
`;

const LogoContainer = styled.div`
  height: inherit;
  display: flex;
  align-items: center;
  padding-left: 30.5px;
`;

const Logo = styled.div`
  background: url(/assets/img/HERE_Logo_2016.png) no-repeat;
  background-size: contain;
  background-position-y: center;
  width: 41px;
  height: 28.5px;
  padding-right: 8px;
  border-right: 1px solid ${({ theme }) => theme.palette.lightGrey};
`;

const AppName = styled.span`
  font-size: 1.14em;
  font-weight: bold;
  margin-left: 10px;
  color: ${({ theme }) => theme.palette.white};
`;

const Ul = styled.ul`
  display: flex;
  margin-bottom: 0;
`;

const Li = styled.li`
  align-items: center;
  justify-content: center;
  align-items: center;
  display: flex;
  height: 50px;
`;

const Link = styled(NavLink)`
  display: flex;
  justify-content: center;
  align-items: center;
  height: inherit;
  text-decoration: none;
  color: ${({ theme }) => theme.palette.texts.whiteTranslucent};
  padding: 0 10px;
  &:hover {
      background-color: ${({ theme }) => theme.palette.secondaryTranslucent};
      color: ${({ theme }) => theme.palette.texts.whiteTranslucent};
  }
`;

const SettingsContainer = styled.div`
  height: inherit;
  margin-left: auto;
  display: flex;
  align-items: center;
  border-left: 1px solid ${({ theme }) => theme.palette.lightGrey};
  padding: 0 30px 0 23px;
  & > div {
      margin-right: 15px;
  }
`;

const MiscSettings = styled.div`
  width: 20px;
  height: 20px;
  background-color: ${({ theme }) => theme.palette.white};
`;

const Support = styled.div`
  width: 20px;
  height: 20px;
  background-color: ${({ theme }) => theme.palette.white};
`;

const AccountSettings = styled.div`
  width: 32px;
  height: 32px;
  border-radius: 50%;
  background-color: ${({ theme }) => theme.palette.white};
`;

type Link = { name: string, to: string };

const renderLinks = (links: Link[]): ReactElement[] => (
  links.map(link => (
    <Li key={link.to}>
      <Link
        id={`navbar.link.${link.to.substring(1)}`}
        // exact should be set to 'true' for main link, otherwise it is always active
        exact={link.to === '/'}
        to={link.to}
        activeStyle={{
          // trick for creating inner border, so text doesn't move
          boxShadow: `inset 0 -3px 0 0 ${COLORS.LIGHT_GREEN}`,
          backgroundColor: `${COLORS.SECONDARY_TRANSLUCENT}`,
          color: `${COLORS.WHITE}`
        }}
      >
        {link.name}
      </Link>
    </Li>
  ))
);

export const AppNavbar = () => {
  const [t]: UseTranslationResponse = useTranslation();
  const links = [
    { name: t('navbar.links.home'), to: '/' },
    { name: t('navbar.links.devices'), to: '/devices' },
    { name: t('navbar.links.device-groups'), to: '/devices-groups' },
    { name: t('navbar.links.software'), to: '/software' },
    { name: t('navbar.links.updates'), to: '/updates' },
    { name: t('navbar.links.campaigns'), to: '/campaigns' },
  ];
  return (
    <Navbar>
      <LogoContainer>
        <Logo />
        <AppName>OTA Connect</AppName>
      </LogoContainer>
      <Ul>
        {renderLinks(links)}
      </Ul>
      <SettingsContainer>
        <MiscSettings />
        <Support />
        <AccountSettings />
      </SettingsContainer>
    </Navbar>
  );
};

export default AppNavbar;
