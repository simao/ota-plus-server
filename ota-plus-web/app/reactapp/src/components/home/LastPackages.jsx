import React, { Component, PropTypes } from 'react';
import { observable } from 'mobx';
import { observer, inject } from 'mobx-react';
import { Loader } from '../../partials';
import _ from 'underscore';
import LastPackagesItem from './LastPackagesItem';
import NoItems from './NoItems';

@inject("stores")
@observer
class LastPackages extends Component {
    render() {
        const { showPackagesCreateModal } = this.props;
        const { packagesStore } = this.props.stores;

        const { lastPackages } = packagesStore;
        return (
            <span style={{height: '100%'}}>
                {packagesStore.packagesFetchAsync.isFetching ?
                    <div className="wrapper-center">
                        <Loader 
                            className="dark"
                        />
                    </div>
                :
                    Object.keys(lastPackages).length ? 
                        _.map(lastPackages, (pack, index) => {
                            return (
                                <LastPackagesItem 
                                    key={index}
                                    pack={pack}
                                />
                            );
                        })
                    :
                        <NoItems 
                            itemName={"package"}
                            create={showPackagesCreateModal}
                        />
                }
            </span>
        );
    }
}

LastPackages.propTypes = {
    stores: PropTypes.object,
}

export default LastPackages;