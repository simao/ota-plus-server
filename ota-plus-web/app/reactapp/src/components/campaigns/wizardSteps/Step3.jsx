import React, {Component, PropTypes} from 'react';
import {observer, inject} from 'mobx-react';
import {WizardGroupsList} from './step3Files';
import {Loader, Form, FormInput} from '../../../partials';
import _ from 'underscore';

@inject("stores")
@observer
class WizardStep3 extends Component {
    constructor(props) {
        super(props);
        this.setWizardData = this.setWizardData.bind(this);
    }

    componentWillMount() {
        const { groupsStore } = this.props.stores;
        groupsStore.fetchWizardGroups();
    }

    setWizardData(groupId) {
        const { groupsStore } = this.props.stores;
        let stepWizardData = this.props.wizardData[3];
        const foundGroup = _.find(stepWizardData.groups, item => item.id === groupId);
        const groupToAdd = _.findWhere(groupsStore.wizardGroups, {id: groupId});
        if (foundGroup)
            stepWizardData.groups.splice(stepWizardData.groups.indexOf(foundGroup), 1);
        else
            stepWizardData.groups.push(groupToAdd);

        if (stepWizardData.groups.length)
            this.props.markStepAsFinished();
        else
            this.props.markStepAsNotFinished();
    }

    render() {
        const { wizardData } = this.props;
        const { groupsStore } = this.props.stores;
        const chosenGroups = wizardData[3].groups;
        return (
            groupsStore.groupsWizardFetchAsync.isFetching ?
                <div className="wrapper-center">
                    <Loader />
                </div>
                :
                <span>
                    <Form>
                        <FormInput
                            label="Select group(s)"
                            showIcon={false}
                            showInput={false}
                        />
                    </Form>
                    <WizardGroupsList
                        chosenGroups={chosenGroups}
                        setWizardData={this.setWizardData.bind(this)}
                    />
                </span>
        );
    }
}

WizardStep3.propTypes = {
    wizardData: PropTypes.object.isRequired,
    stores: PropTypes.object
}

export default WizardStep3;

