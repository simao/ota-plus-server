import React, { useState } from 'react';
import { Dropdown } from 'antd';
import { useTranslation } from 'react-i18next';
import { useObserver } from 'mobx-react';
import {
  ButtonStyled,
  ButtonText,
  DropdownIcon,
  FilterTitle,
  ListDate,
  ListDescription,
  ListIcon,
  ListItem,
  ListNoDataContainer,
  ListNoDataDescription,
  ListNoDataIcon,
  ListNoDataWrapper,
  ListStyled,
  ListTitle,
  MenuCheckbox,
  MenuItemStyled,
  MenuSelectedLength,
  MenuStyled,
  RecentActivityWrapper,
  RightContainer,
  Title,
  TopContainer
} from './styled';
import { NO_ITEMS_ICON } from '../../../../config';
import {
  getDeviceGroupListIcon,
  getListDescription,
  getListIcon,
  sendFilterLatestAction,
  sendSeeLatestAction,
} from '../../../../helpers/recentActivityHelper';
import { ACTIVITIES_TYPE } from '../../../../constants';
import { useStores } from '../../../../stores/hooks';
import { Loader } from '../../../../partials';

const MENU_ITEMS = [
  { type: ACTIVITIES_TYPE.DEVICE, title: 'dashboard.recent-activity.filter-menu.item-devices' },
  { type: ACTIVITIES_TYPE.SOFTWARE_VERSION, title: 'dashboard.recent-activity.filter-menu.item-software-versions' },
  { type: ACTIVITIES_TYPE.DEVICE_GROUP, title: 'dashboard.recent-activity.filter-menu.item-device-groups' },
  { type: ACTIVITIES_TYPE.SOFTWARE_UPDATE, title: 'dashboard.recent-activity.filter-menu.item-software-updates' },
  { type: ACTIVITIES_TYPE.CAMPAIGN, title: 'dashboard.recent-activity.filter-menu.item-campaigns' }
];

const filterMenu = (t, menuItemsSelected, handleMenuChange) => (
  <MenuStyled id="recent-activity-filter-menu" onClick={handleMenuChange}>
    {MENU_ITEMS.map((item, index) => (
      <MenuItemStyled id={`recent-activity-filter-menu-item-${index}`} key={`${index}`}>
        <MenuCheckbox id={`recent-activity-filter-menu-item-checkbox-${index}`} checked={menuItemsSelected[index]} />
        {t(MENU_ITEMS[index].title)}
      </MenuItemStyled>
    ))}
  </MenuStyled>
);

function useStoreData() {
  const { stores } = useStores();
  return useObserver(() => ({
    devicesTotalCount: stores.devicesStore.devicesTotalCount,
    isFetching: stores.recentlyCreatedStore.recentlyCreatedFetchAsync.isFetching,
    recentlyCreated: stores.recentlyCreatedStore.recentlyCreated,
  }));
}

const RecentActivity = () => {
  const [menuItemsSelected, setMenuItemsSelected] = useState(MENU_ITEMS.map(() => true));
  const [menuSelectedLength, setMenuSelectedLength] = useState(MENU_ITEMS.length);
  const [filterMenuVisible, setFilterMenuVisible] = useState(false);
  const { devicesTotalCount, isFetching, recentlyCreated } = useStoreData();
  const { t } = useTranslation();
  const { stores } = useStores();
  const handleMenuChange = (event) => {
    const key = parseInt(event.key, 10);
    menuItemsSelected[key] = !menuItemsSelected[key];
    if (menuItemsSelected[key]) {
      const menuItemType = MENU_ITEMS[key].type;
      sendFilterLatestAction(menuItemType);
    }
    setMenuItemsSelected(menuItemsSelected);
    const selectedLength = menuItemsSelected.filter(item => item).length;
    setMenuSelectedLength(selectedLength);
    const selectedItems = MENU_ITEMS.filter((item, index) => menuItemsSelected[index]);
    const params = selectedItems.map(item => item.type);
    params.forEach((item) => {
      sendSeeLatestAction(item);
    });
    stores.recentlyCreatedStore.fetchRecentlyCreated(params);
  };

  return (
    <RecentActivityWrapper id="recent-activity-wrapper" empty={recentlyCreated.length === 0}>
      <TopContainer id="recent-activity-top-container">
        <Title id="docs-links-title">{t('dashboard.recent-activity.title')}</Title>
        {devicesTotalCount > 0 && (
          <RightContainer>
            <FilterTitle id="recent-activity-filter-title">
              {t('dashboard.recent-activity.filter-menu.title')}
            </FilterTitle>
            <Dropdown
              id="recent-activity-filter-dropdown"
              onVisibleChange={(visible) => {
                setFilterMenuVisible(visible);
              }}
              overlay={filterMenu(t, menuItemsSelected, handleMenuChange)}
            >
              <ButtonStyled id="recent-activity-filter-button">
                <ButtonText>{t('dashboard.recent-activity.filter-menu.type')}</ButtonText>
                <MenuSelectedLength>{menuSelectedLength}</MenuSelectedLength>
                <DropdownIcon type={filterMenuVisible ? 'up' : 'down'} />
              </ButtonStyled>
            </Dropdown>
          </RightContainer>
        )}
      </TopContainer>
      { recentlyCreated.length && devicesTotalCount > 0 && !isFetching ? (
        <ListStyled
          id="recent-activity-list"
          dataSource={recentlyCreated}
          renderItem={({ date, title, type, groupType }, index) => (
            <ListItem id={`recent-activity-list-item-${index}`} key={index}>
              <ListIcon
                src={type === ACTIVITIES_TYPE.DEVICE_GROUP
                  ? getDeviceGroupListIcon(groupType)
                  : getListIcon(type)
                }
              />
              <div>
                <ListTitle>{title}</ListTitle>
                <ListDescription>{getListDescription(t, type)}</ListDescription>
              </div>
              <ListDate>{date}</ListDate>
            </ListItem>
          )}
        />
      ) : (
        <ListNoDataWrapper id="recent-activity-no-data-wrapper">
          <ListNoDataContainer id="recent-activity-no-data-container">
            {isFetching ? <Loader id="recent-activity-no-data-loader" /> : (
              <div>
                <ListNoDataIcon src={NO_ITEMS_ICON} />
                <ListNoDataDescription id="recent-activity-no-data-description">
                  { devicesTotalCount === 0
                    ? t('dashboard.recent-activity.no-data.nothing-created')
                    : t('dashboard.recent-activity.no-data.empty')
                  }
                </ListNoDataDescription>
              </div>
            )}
          </ListNoDataContainer>
        </ListNoDataWrapper>
      )}
    </RecentActivityWrapper>
  );
};

export default RecentActivity;
