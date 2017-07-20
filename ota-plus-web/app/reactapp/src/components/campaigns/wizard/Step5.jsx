import React, { Component, PropTypes } from 'react';
import { observable, observe } from 'mobx';
import { observer } from 'mobx-react';
import _ from 'underscore';
import { WizardPackagesVersionList } from './step5';
import { VelocityTransitionGroup } from 'velocity-react';
import { Loader } from '../../../partials';
import { SelectField, MenuItem } from 'material-ui';

const headerHeight = 28;

@observer
class WizardStep5 extends Component {
    constructor(props) {
        super(props);
        this.sortByFirstLetter = this.sortByFirstLetter.bind(this);
    }
    componentWillMount() {
        this.props.hardwareStore.fetchHardwareIds();
    }
    sortByFirstLetter(packagesList) {
        let sortedPackages = {};
        _.each(packagesList, (pack, index) => {
            let firstLetter = pack.packageName.charAt(0).toUpperCase();
            firstLetter = firstLetter.match(/[A-Z]/) ? firstLetter : '#';
            if(_.isUndefined(sortedPackages[firstLetter])) {
                sortedPackages[firstLetter] = [];
            }
            sortedPackages[firstLetter].push(pack);
        });
        return sortedPackages;
    }
    render() {
        const { wizardData, selectVersion, markStepAsFinished, markStepAsNotFinished, hardwareStore } = this.props;
        let chosenPackagesList = this.sortByFirstLetter(wizardData[0].packages);
        let selectedVersions = wizardData[1].versions;
        return (
            <div className="ios-list" ref="list">
                <span>
                    {hardwareStore.hardwareIdsFetchAsync.isFetching ?
                        <Loader />
                    :
                        <span>
                            {_.map(chosenPackagesList, (packages, letter) => {
                                let packsCount = packages.length;
                                return (
                                    <span key={letter}>
                                        <div className="header">
                                            {letter}
                                        </div>
                                        {_.map(packages, (pack, index) => {
                                             return (
                                                <span key={index}>
                                                    <WizardPackagesVersionList
                                                        pack={pack}
                                                        packsCount={packsCount}
                                                        selectedVersions={selectedVersions}
                                                        selectVersion={selectVersion}
                                                        markStepAsFinished={markStepAsFinished}
                                                        markStepAsNotFinished={markStepAsNotFinished}
                                                        hardwareStore={hardwareStore}
                                                    />
                                                </span>
                                            );
                                        })}
                                    </span>
                                );
                            })}
                        </span>
                    }
                </span>                
            </div>
        );
    }
}

WizardStep5.propTypes = {
    setWizardData: PropTypes.func.isRequired,
    packagesStore: PropTypes.object.isRequired,
    hardwareStore: PropTypes.object
}

export default WizardStep5;