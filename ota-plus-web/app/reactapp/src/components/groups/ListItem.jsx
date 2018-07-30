import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { observable } from 'mobx';
import { translate } from 'react-i18next';
import { DropTarget } from 'react-dnd';
import CreateModal from './CreateModal';
import { Dropdown } from '../../partials';

const groupTarget = {
    drop(props, monitor) {
        const device = monitor.getItem();
        props.onDeviceDrop(device, props.group.id);
    },
};

function collect(connect, monitor) {
    return {
        connectDropTarget: connect.dropTarget(),
        isOver: monitor.isOver(),
        canDrop: monitor.canDrop(),
    };
}

@observer
class ListItem extends Component {
    @observable createModalShown = false;
    @observable showEdit = false;

    constructor(props) {
        super(props);
        this.showCreateModal = this.showCreateModal.bind(this);
        this.hideCreateModal = this.hideCreateModal.bind(this);
        this.showDropdown = this.showDropdown.bind(this);
        this.hideDropdown = this.hideDropdown.bind(this);
    }

    showCreateModal(e) {
        if(e) e.stopPropagation();
        this.createModalShown = true;
    }

    hideCreateModal(e) {
        if(e) e.preventDefault();
        this.createModalShown = false;
    }

    showDropdown() {
        this.showEdit = true;
    }

    hideDropdown() {
        this.showEdit = false;
    }

    render() {
        const { t, group, isSelected, selectGroup, groupsStore } = this.props;
        const { isOver, canDrop, connectDropTarget } = this.props;
        return (
            connectDropTarget(
                <div
                    title={group.groupName}
                    className={"groups-panel__item" + (isSelected ? " groups-panel__item--selected" : "") + (isOver ? " groups-panel__item--active" : "")}
                    id={"button-group-" + group.groupName}>
                    {groupsStore.activeFleet ?
                        <div className="groups-panel__item-icon groups-panel__item-icon--fleet">
                            {group.groupName.substring(0, 3)}
                        </div>
                    :
                        <div className={"groups-panel__item-icon groups-panel__item-icon--default" + (isSelected ? " groups-panel__item-icon--active" : "")}></div>
                    }
                    <div className="groups-panel__item-desc" onClick={() => {
                        selectGroup({type: 'real', groupName: group.groupName, id: group.id});
                    }}>
                        <div className="groups-panel__item-title">
                            <div className="groups-panel__item-title-value">
                                {group.groupName}
                            </div>
                            <div className="dots" onClick={this.showDropdown} id={"group-" + group.groupName + "-dropdown"}>
                                <span></span>
                                <span></span>
                                <span></span>
                            </div>
                            <Dropdown show={this.showEdit} hideSubmenu={this.hideDropdown}>
                                <li className="package-dropdown-item">
                                    <a className="package-dropdown-item" href="#" id="edit-comment"
                                       onClick={(e) => {
                                           e.preventDefault();
                                           selectGroup({type: 'real', groupName: group.groupName, id: group.id});
                                           this.showCreateModal();
                                       }}>
                                        <img src="/assets/img/icons/edit_icon.svg" alt="Icon" />
                                        Edit name
                                    </a>
                                </li>
                            </Dropdown>
                        </div>
                        <div className="groups-panel__item-subtitle" id={"group-" + group.groupName + '-devices'}>
                            {t('common.deviceWithCount', {count: group.devices.total})}
                        </div>
                    </div>
                    {this.createModalShown ?
                        <CreateModal
                            shown={this.createModalShown}
                            hide={this.hideCreateModal}
                            groupsStore={groupsStore}
                            selectGroup={selectGroup}
                            action="rename"
                            modalTitle="Edit Group"
                            buttonText="Save"
                        />
                    :
                        null
                    }
                </div>
            )
        );
    }
}

ListItem.propTypes = {
    group: PropTypes.object.isRequired,
    isSelected: PropTypes.bool.isRequired,
    selectGroup: PropTypes.func.isRequired,
    connectDropTarget: PropTypes.func.isRequired,
    isOver: PropTypes.bool.isRequired,
    canDrop: PropTypes.bool.isRequired,
    onDeviceDrop: PropTypes.func.isRequired,
}

export default translate()(DropTarget('device', groupTarget, collect)(ListItem));