import React, { Component, PropTypes } from 'react';
import { observer } from 'mobx-react';
import _ from 'underscore';
import { Loader } from '../../../partials';
import MtuListItem from './MtuListItem';
import { InfiniteScroll } from '../../../utils';

@observer
class MtuList extends Component {
    constructor(props) {
        super(props);
    }
    render() {
        const { packagesStore, device } = this.props;
        const emptyHistory = (
            <div className="wrapper-center">
                Multi target update history is empty.
            </div>
        );
        return (
            <ul className="queue-modal__list">
                <InfiniteScroll
                    className="wrapper-infinite-scroll"
                    hasMore={packagesStore.packagesHistoryCurrentPage < packagesStore.packagesHistoryTotalCount / packagesStore.packagesHistoryLimit}
                    isLoading={packagesStore.packagesHistoryFetchAsync.isFetching}
                    useWindow={false}
                    loadMore={() => {
                        packagesStore.fetchPackagesHistory(device.uuid, packagesStore.packagesHistoryFilter)
                    }}
                >
                    {packagesStore.packagesHistory.length ?
                        _.map(packagesStore.packagesHistory, (historyItem, index) => {
                            return (
                                <MtuListItem
                                    item={historyItem}
                                    key={index}
                                />
                            );
                        })
                    :
                        emptyHistory
                    }
                </InfiniteScroll>
            </ul>
        );
    }
}

MtuList.propTypes = {
    packagesStore: PropTypes.object.isRequired,
    device: PropTypes.object.isRequired
}

export default MtuList;