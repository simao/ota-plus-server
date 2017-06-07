const WebsocketHandler = (function (stores) {
    const base = this;
    this.init = function () {
        const proto = (location.protocol == "http:") ? "ws://" : "wss://";
        const port = (location.protocol == "http:") ? ":" + location.port : ":8080";
        this.websocket = new WebSocket(proto + location.hostname + port + "/api/v1/events/ws");

        this.websocket.onopen = function () {
            console.log("WEBSOCKET: OPEN");
        };

        this.websocket.onmessage = function (msg) {
            const eventObj = JSON.parse(msg.data);
            const type = eventObj.type;
            const data = eventObj.event;
            switch (type) {
                case "DeviceSeen":
                    stores.devicesStore.fetchInitialDevices();
                    if(stores.devicesStore.onlineDevices.length === 0 || stores.devicesStore.onlineDevices.length === 1 && !stores.fireworksPageAcknowledged) {
                        window.location = '#/fireworks'
                    }
                    stores.devicesStore._updateDeviceData(data.uuid, {lastSeen: data.lastSeen});
                    stores.packagesStore.fetchInitialDevicePackages(data.uuid);
                    stores.hardwareStore.fetchHardware(data.uuid);
                    break;
                case "DeviceUpdateStatus":
                    stores.devicesStore._updateDeviceData(data.device, {deviceStatus: data.status});
                    break;
                case "DeviceCreated":
                    break;
                case "PackageCreated":
                    stores.packagesStore._addPackage(data);
                    break;
                case "PackageBlacklisted":
                    stores.packagesStore._blacklistPackage(data.id);
                    break;
                case "UpdateSpec":
                    if(stores.packagesStore.deviceQueue.length && stores.devicesStore.device.uuid === data.device) {
                        if(data.status !== 'Pending') {
                            stores.packagesStore.fetchDevicePackagesQueue(data.device);
                        }
                        if(data.status === 'Finished') {
                            stores.packagesStore.fetchDevicePackagesHistory(data.device);
                            stores.packagesStore.fetchDevicePackagesUpdatesLogs(data.device);
                            stores.packagesStore.fetchOndevicePackages(data.device, null);
                        }
                    } else {
                        if(data.status === 'Finished') {
                            if(Object.keys(stores.campaignsStore.campaign).length) {
                                stores.campaignsStore.fetchCampaign(stores.campaignsStore.campaign.meta.id);
                            }
                            stores.campaignsStore.fetchCampaigns();
                        }
                    }
                    break;
                default:
                    console.log('Unhandled event type: ' + eventObj.type);
                    break;
            }
        };

        this.websocket.onclose = function (msg) {
            console.log('WEBSOCKET: CLOSE');
            if (msg.code === 1006) {
                base.init();
            }
        };

        this.websocket.onerror = function (msg) {
            console.log('WEBSOCKET: ERROR');
            console.log(msg);
        };
    };

    this.destroy = function () {
        this.websocket.close();
    };
});

export default WebsocketHandler;