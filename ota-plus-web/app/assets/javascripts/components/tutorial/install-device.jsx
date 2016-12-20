define(function(require) {
  var React = require('react'),
      Router = require('react-router'),
      Link = Router.Link,
      VelocityTransitionGroup = require('mixins/velocity/velocity-transition-group'),
      ModalTooltip = require('../modal-tooltip');
        
  class InstallDevice extends React.Component {
    constructor(props) {
      super(props);
      this.state = {
        shownTooltipInfoName: null,
        tutorialHeight: 300
      };
      this.showTooltipInfo = this.showTooltipInfo.bind(this);
      this.hideTooltipInfo = this.hideTooltipInfo.bind(this);
      this.setTutorialHeight = this.setTutorialHeight.bind(this);
    }
    componentDidMount() {
      window.addEventListener("resize", this.setTutorialHeight);
      this.setTutorialHeight();
    }
    componentWillUnmount() {
      window.removeEventListener("resize", this.setTutorialHeight);
    }
    showTooltipInfo(name, e) {
      if(e) e.preventDefault();
      this.setState({shownTooltipInfoName: name});
    }
    hideTooltipInfo(e) {
      if(e) e.preventDefault();
      this.setState({shownTooltipInfoName: null});
    }
    setTutorialHeight() {
      var windowHeight = jQuery(window).height();
      this.setState({
        tutorialHeight: windowHeight - jQuery('.grey-header').offset().top - jQuery('.grey-header').outerHeight()
      });
    }
    render() {
      var copyProcessTooltipContent = (
        <div className="text-center">
          For debian packages, you can install from the command line with <br />
          <pre>dpkg -i ota-plus-client-[version].deb</pre> <br /><br />
          RPM packages can be installed from the command line with <br />
          <pre>rpm -i ota-plus-client-[version].rpm</pre>
        </div>
      );
      var otherSystemTooltipContent = (
        <div className="text-center">
          The pre-built packages register the OTA Plus Client to start with systemd. <br /><br />
          If you use another init system, you'll need to install ota-plus-client manually.
        </div>
      );
      return (
        <div>
          <div className="tutorial-install-device" style={{height: this.state.tutorialHeight}}>
            <div className="center-xy text-center">
              <div className="font-24 white"><strong>Device never seen online.</strong></div>
              <div className="font-16">You need to install the client on your device.</div>
              
              <div className="inner">
                <div className="common-steps">
                  <div className="steps-row">
                    <div className="lane first-lane">
                      <div className="step first-step">
                        <div className="lane-name">
                          Fast Lane
                        </div>
                        <div className="step-inner">
                          <div className="step-no">1.</div>
                          <div className="step-desc">
                            Download the OTA Plus Client for<br />
                            your distro/system architecture.
                            <div className="margin-top-20">
                              <a href={"/api/v1/client/" + this.props.deviceUUID + "/deb/64"} className="btn btn-confirm btn-ota-client" target="_blank">DEB Intel 64</a>
                              <a href={"/api/v1/client/" + this.props.deviceUUID + "/rpm/64"} className="btn btn-confirm btn-ota-client" target="_blank">RPM Intel 64</a>
                            </div>
                          </div>
                        </div>
                      </div>
                      <div className="step second-step">
                        <div className="step-inner">
                          <div className="step-no">2.</div>
                          <div className="step-desc">
                            Copy the Package to your device, and  <br />
                            install it using your package manager.
                            <div className="margin-top-20"><a href="#" onClick={this.showTooltipInfo.bind(this, 'copy_process')} className="font-12 color-main"><i className="fa fa-cog" aria-hidden="true"></i> How do I do that?</a></div>
                            <div><a href="#" onClick={this.showTooltipInfo.bind(this, 'other_system')} className="font-12 color-main"><i className="fa fa-cog" aria-hidden="true"></i> I use an init system other than systemd.</a></div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                  <div className="step-divider">
                    or
                  </div>
                  <div className="steps-row">
                    <div className="lane second-lane">
                      <div className="step first-step">
                        <div className="lane-name">
                          Nerd Lane
                        </div>
                        <div className="step-inner">
                          <div className="step-no">1.</div>
                          <div className="step-desc">
                            Download the unique<br />
                            credentials for this device.
                            <div className="margin-top-20">
                              <a href={"/api/v1/client/" + this.props.deviceUUID + "/toml/64"} className="btn btn-confirm" target="_blank">Download</a>
                            </div>
                          </div>
                        </div>
                      </div>
                      <div className="step second-step">
                        <div className="step-inner">
                          <div className="step-no">2.</div>
                          <div className="step-desc">
                            Manually build and install the<br />
                            open source OTA client.
                            <div className="margin-top-20"><a href="http://advancedtelematic.github.io/rvi_sota_server/cli/building-the-sota-client.html" className="font-12 color-main" target="_blank"><i className="fa fa-cog" aria-hidden="true"></i> How to build manually?</a></div>
                            <div><a href="http://advancedtelematic.github.io/rvi_sota_server/cli/client-startup-and-configuration.html" className="font-12 color-main" target="_blank"><i className="fa fa-cog" aria-hidden="true"></i> How to install the client?</a></div>
                          </div>
                        </div>
                      </div>
                    </div>
                  </div>
                </div>
                <div className="last-step">
                  <div className="center-xy">
                    <img src="/assets/img/icons/check.png" alt="" />
                    <div className="margin-top-20">
                      Your new device should now <br />
                      appear online!
                    </div>
                  </div>
                </div>
              </div>
            </div>
          </div>
          
          <VelocityTransitionGroup enter={{animation: "fadeIn"}} leave={{animation: "fadeOut"}}>
            {this.state.shownTooltipInfoName === 'copy_process' ?
              <ModalTooltip 
                title="How to copy the Package to your device"
                body={copyProcessTooltipContent}
                confirmButtonAction={this.hideTooltipInfo}/>
            : undefined}
          </VelocityTransitionGroup>
          <VelocityTransitionGroup enter={{animation: "fadeIn"}} leave={{animation: "fadeOut"}}>
            {this.state.shownTooltipInfoName === 'other_system' ?
              <ModalTooltip 
                title="How to use init system other than systemd"
                body={otherSystemTooltipContent}
                confirmButtonAction={this.hideTooltipInfo}/>
            : undefined}
          </VelocityTransitionGroup>
        </div>
      );
    }
  };

  return InstallDevice;
});
