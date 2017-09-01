import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import { Pie } from 'react-chartjs';
import { translate } from 'react-i18next';

@observer
class TufGroupsListItem extends Component {
    constructor(props) {
        super(props);
    }
    render() {
        const { t, group, statistics, showCancelGroupModal, foundGroup, campaign } = this.props;
        const progress = Math.min(Math.round(statistics.processed/Math.max(foundGroup.devices.total, 1) * 100), 100);
        const data = [
            {
              value: statistics.affected,
              color:"#FE0001",
              highlight: "#FF0000",
              label: "Failure rate"
            },
            {
              value: statistics.processed,
              color: "#83D060",
              highlight: "#96DCD1",
              label: "Success rate"
            },
            {
              value: 0,
              color: "#CCCCCC",
              highlight: "#CCCCCC",
              label: "Cancelled rate"
            }
        ];
        return (
            <div className="row display-flex">
                <div className="name col-xs-3">
                    <div className="element-box group">
                        <div className="icon"/>
                        <div className="desc">
                            <div className="title">
                                {foundGroup.groupName}
                            </div>
                            <div className="subtitle">
                                {t('common.deviceWithCount', {count: foundGroup.devices.total})}
                            </div>
                        </div>
                    </div>
                </div>
                <div className="stats col-xs-6">
                    <div className="devices-progress">
                        <div className="progress progress-blue">
                            <div className={"progress-bar" + (campaign.statistics.status !== 'finished' ? ' progress-bar-striped active': '')}
                                 role="progressbar"
                                 style={{width: campaign.statistics.status !== 'finished' ? '0%' : (foundGroup.devices.total !== 0 ? progress + '%' : '100%')}}>
                                <div className="wrapper-rate">
                                    {campaign.statistics.status !== 'finished'
                                        ? '0%'
                                        : (foundGroup.devices.total !== 0
                                            ? progress + '%'
                                            : '100%')
                                    }
                                    <i className="fa fa-check" aria-hidden="true" />
                                </div>
                            </div>
                        </div>
                    </div>
                </div>
                <div className="col-xs-3">
                    <div className="status-group display-flex">
                        <p>{statistics.processed} processed, {statistics.affected} affected</p>
                    </div>
                </div>
            </div>
        );
    }
}

TufGroupsListItem.propTypes = {
    group: PropTypes.string.isRequired,
    foundGroup: PropTypes.object.isRequired,
    statistics: PropTypes.object.isRequired,
    showCancelGroupModal: PropTypes.func.isRequired
}

export default translate()(TufGroupsListItem);