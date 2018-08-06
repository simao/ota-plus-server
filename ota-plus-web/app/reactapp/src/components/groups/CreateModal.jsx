import React, { Component, PropTypes } from 'react';
import { observable, intercept, ObservableMap } from 'mobx';
import { observer, inject } from 'mobx-react';
import { Modal, AsyncResponse, Form, FormInput } from '../../partials';
import { FormsyText } from 'formsy-material-ui/lib';
import { FlatButton } from 'material-ui';
import serialize from 'form-serialize';
import { AsyncStatusCallbackHandler } from '../../utils';
import {
    Step1,
    Step2,
} from './createWizard';
import _ from 'underscore';

const wizardSteps = [
    {
        class: Step1,
        name: "Choose group type",
        isFinished: false,
    },
    {
        class: Step2,
        name: "Pass group data",
        isFinished: false,
    },
];

const initialCurrentStepId = 0;

@inject('stores')
@observer
class CreateModal extends Component {
    @observable currentStepId = initialCurrentStepId;
    @observable steps = wizardSteps;
    @observable groupType = '';

    constructor(props) {
        super(props);
        const { groupsStore } = props.stores;
        this.createHandler = new AsyncStatusCallbackHandler(groupsStore, 'groupsCreateAsync', this.handleResponse.bind(this));
        this.markStepAsFinished = this.markStepAsFinished.bind(this);
        this.markStepAsNotFinished = this.markStepAsNotFinished.bind(this);
        this.selectGroupType = this.selectGroupType.bind(this);
        this.nextStep = this.nextStep.bind(this);
        this.verifyIfPreviousStepsFinished = this.verifyIfPreviousStepsFinished.bind(this);
        this.createGroup = this.createGroup.bind(this);
        this.isLastStep = this.isLastStep.bind(this);
    }
    componentWillMount() {
        const { featuresStore } = this.props.stores;
        const { alphaPlusEnabled } = featuresStore;
        if(!alphaPlusEnabled) {
            this.currentStepId = 1;
            this.groupType = 'static';
        }
    }
    markStepAsFinished() {
        this.steps[this.currentStepId].isFinished = true;
    }
    markStepAsNotFinished() {
        this.steps[this.currentStepId].isFinished = false;
    }
    verifyIfPreviousStepsFinished(stepId) {
        if (_.find(this.steps, function (step, index) {
                return index <= stepId && step.isFinished === false;
            }))
            return false;
        return true;
    }
    isLastStep() {
        return this.currentStepId == this.steps.length - 1;
    }
    nextStep() {
        if (this.verifyIfPreviousStepsFinished(this.currentStepId) && this.currentStepId != this.steps.length - 1) {
            this.currentStepId = this.currentStepId + 1;
        }
    }
    selectGroupType(type) {
        this.groupType = type;
        this.markStepAsFinished();
    }
    componentWillUnmount() {
        this.createHandler();
    }
    createGroup() {
        if(this.groupType === 'static') {
            const { groupsStore } = this.props.stores;
            let data = serialize(document.querySelector('#static-group-create-form'), { hash: true });
            groupsStore.createGroup(data.groupName);
        } else {
            console.log('CREATING AUTO GROUP');
        }
    }
    handleResponse() {
        const { groupsStore } = this.props.stores;
        let data = null;
        if(this.groupType === 'static') {
            data = serialize(document.querySelector('#static-group-create-form'), { hash: true });
        } else {
            data = serialize(document.querySelector('#automatic-group-create-form'), { hash: true });
        }
        this.props.selectGroup({type: 'real', groupName: data.groupName, id: groupsStore.latestCreatedGroupId});
        groupsStore._prepareGroups(groupsStore.groups);
        this.props.hide();
    }
    render() {
        const { shown, hide } = this.props;
        const currentStep = this.steps[this.currentStepId];
        const step = (
            <span>
                {React.createElement(currentStep.class, {
                    selectGroupType: this.selectGroupType,
                    groupType: this.groupType,
                    markStepAsFinished: this.markStepAsFinished,
                    markStepAsNotFinished: this.markStepAsNotFinished,
                })}
                <div className="body-actions">
                    {this.isLastStep() ?
                        <button
                            className="btn-primary"
                            id="wizard-launch-button"
                            onClick={this.createGroup}
                            disabled={!currentStep.isFinished}
                        >
                            Create
                        </button>
                    :
                        <button
                            disabled={!currentStep.isFinished}
                            className="btn-primary"
                            id="next"
                            onClick={this.nextStep}
                        >
                            Next
                        </button>
                    }
                </div>
            </span>
        );
        return (
            <Modal 
                title={
                    <div>
                        Create new group
                    </div>
                }
                topActions={
                    <div className="top-actions flex-end">
                        <div className="modal-close" onClick={hide}>
                            <img src="/assets/img/icons/close.svg" alt="Icon" />
                        </div>
                    </div>
                }
                className="create-group-modal"
                content={step}
                shown={shown}
            />
        );
    }
}

CreateModal.propTypes = {
    shown: PropTypes.bool.isRequired,
    hide: PropTypes.func.isRequired,
    selectGroup: PropTypes.func.isRequired,
    stores: PropTypes.object
}

export default CreateModal;