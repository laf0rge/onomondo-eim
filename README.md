# onomondo-eim
Remote provisioning and management of the eUICC in IoT Devices as defined in eSIM IoT Architecture and Requirements SGP.31.


Getting Started
---------------

### Installing run time dependencies

* Dependencies required to compile and run the onomondo_eim erlang application:
  apt-get install erlang
  apt-get install curl
  apt-get install build-essential

* Dependencies required to compile and run the restop.py tool
  apt-get install python3-full
  pip3 install erlang-py
  pip3 install requests


### Building and Running

The build process is started using `make`. The overall process may take a while since erlang.mk will also take care of
downloading and compiling further erlang related dependencies. The build process can also be started with `make run`.
This will start the application immediately after finishing the compilation.

Once the application is running it will display a prompt. To exit the application type `CTRL+G` and then `q`.


Configuration
-------------

The user relevant configuration files can be found in the `./config` directory. In the default configuration
onomondo-eim will start with all services bound to local host. The most relevant configuration file is
`sys.config`, which sets the application parameters. The file `vm.args` sets erlang VM parameters. In most
cases those parameters do not have to be modified.

### sys.config

* esipa_ip: Configure on which network interface the ESipa interface should listen.
* esipa_port: Configure the port number where the ESipa interface should listen.
* esipa_ssl_disable: Set this to true to use HTTP instead of HTTPs. This is a debug feature intended for lab setups to
  simplify the creation of protocol traces.
* esipa_ssl_cert: Configure the location of the SSL certificate.
* esipa_ssl_key: Configure the location of the SSL private key.
* rest_ip: Configure on which network interface the REST API interface should listen.
* rest_port: Configure the port number where the REST API interface should listen.
* eim_id: Configure the eimId of the eIM instance.
* es9p_ssl_disable: Set this to true to use HTTP instead of HTTPs. This is a debug feature intended for lab setups to
  simplify the creation of protocol traces.
* eim_cert: Configure the location of the certificate that is used for verification of eUICC packages.
* eim_key: Configure the location of the private key that is used for signing eUICC packages.
* counter_value: Set the start value of the replay protection counter (eUICC packages).
* rest_timeout_stuck: Configure timeout until an order/procedure (e.g. profile download) must finish.
* rest_timeout_noshow: Configure timeout until an order/procedure must start.
* rest_timeout_expired: Configure timeout until the REST API user must lookup/delete the order via the REST API

#### timeout behavior

The three REST API related timeouts (rest_timeout_) serve the purpose that the underlying REST database of the REST
API won't overflow over time in case REST API users fail to monitor their orders and most importantly delete their
orders when done.

* The timeout rest_timeout_stuck gurds against stuck orders. Orders may get stuck due to communication errors between
  SMDP+ or IPAd. When a procedure is stuck for too long it gets marked as done and an appropriate error code is
  communicated to the REST API user. Since the overall time a procedure usually won't take more than a few minutes
  (usually below one minute) 300 sec. would be a good compromise here.

* The timeout rest_timeout_noshow guards against IPAd/eUICCs that fail to show up. When an order is placed the IPAd
  is expected to poll the eIM within a reasonable amount of time. When the IPAd fails to poll for some reason the
  order gets marked as done and an appropriate error code is communicated to the REST API user. Usually the polling
  is triggered through some side channel or the polling happens regularly. Depending on the situation the timeout may
  be set to several hours or even days. In any case it must not be lower than rest_timeout_stuck for obvious reasons.
  A recommended timeout value would be 1800 (30 min).

* The timeout rest_timeout_expired guards against careless REST API users. As mentioned already, the REST API user is
  expected to carefully monitor his orders and delete them on his own responsibility. However, it may be that a REST
  API user fails to monitor an order he created. To prevent the database from gradually overflowing such orders will
  be expired. This means they are deleted silently. Depending on the situation and the quality of the tooling that
  operates the REST API, this timeout can be set generously (hours, days or even weeks). In any case the timeout must
  not be set lower than rest_timeout_noshow for obvious reasons. A recommended timeout value would be 86400 (24h)

### vm.args

* See also: https://www.erlang.org/doc/man/erl.html
* mnesia dir: configure the location of the mnesia database. The default setting will store the database in
  `./_rel/onomondo_eim_release/db`.


REST API
--------

The REST API offered by onomondo-eim is a powerful interface to manage a fleet of eUICCs. The REST API lets the user
trigger profile downloads and offers full access to all PSMOs and ECOs that are specified in SGP.32.

### Facilities

The REST API is devided up into so called "facilities". The facility identifier is the first path element of the HTTP URL. There are four facilities defined:

* download: management of profile downloads
* psmo: Profile State Management Operations
* eco: Eim Configuration Operations
* euicc: eim-local eUICC configuration Operations

example: http://127.0.0.1:8080/download/...

### Operations

The REST API defines four different basic operations. The name of the operation is the second path element of the HTTP URL:

* create: create a rest resource, returns resourceId
* lookup: lookup a rest resource by its resourceId, returns JSON
* delete: delete a rest resource by its resourceId, returns JSON
* list: list all resource IDs available for the current facility, returns JSON.

example: http://127.0.0.1:8080/download/create

### JSON interface

The REST API receives JSON formatted data via HTTP POST requests and returns a JSON formatted response.

#### Creating orders

Each request contains the eidValue and a so called "order". The eidValue identifies the eUICC and the order contains
information that the eIM needs to fullfill a specific task (e.g. trigger a profile download).

Example: { "eidValue" : "89882119900000000000000000000005", "order" : {"activationCode" : "1$testsmdpplus1.example.com$OPxLD-UVRuC-jysPI-YkOwT"}}'

#### Monitoring orders

The REST-API user is expected to poll the rest resource from time to time to check on its status. The polling is done
using the lookup operation:

Example: http://127.0.0.1:8080/download/lookup/8a901bd9-f203-4eae-bcba-12dee32f4444

The result will contain a status field along with some other information that allows the API user to monitor the
progress and the outcome of the current order.

The following fields are defined:

* status: contains the processing status of an order. This field does not say anything about success or failure of an
  order. It just tells the status. When an order is new, the status will be "new". An order that is currently in
  progress will report "work" as status. When the order is finished, the reported status will be "done". In case of an
  resource error (non existing resourceId) the status will be "absent". Contrary to all other fields, the status field
  is a mandatory field that is always present.
* timestamp: contain the timestamp of the last update. The timestamp may be used by the REST API user to get an idea
  how long orders take.
* resource: The resource is a copy of the JSON object that was submitted when the order was created. It contains the
  eidValue and the order. It is included in the data so that the REST API user does not have to memorize it.
* outcome: The outcome depends on the facility and on specific order. Its purpose is to inform the REST API user of
  the order results. The outcome is modeled as a list since an outcome may contain more then one result. This is in
  particular true for eUICC packages with more than one PSMO or eCO. The outcome is also used to convey error codes
  back to the API user in case there were problems during the execution of the order.
* debuginfo: The debug info contains the last ESipa message that was received from the IPAd in erlang ETF format.
  During normal operation the field has no relevance. Its only purpose is to provide debug information to a software
  engineer. It should also be noted that the structure of the contents of this field may changed without further
  notice.

Example, normal response: {"status": "done", "timestamp": "1718881629", "resource": {"eidValue": "89086030202200000022000027485428", "order": {"download": {"activationCode": "1$rsp.truphone.com$QR-G-5C-1LS-1W1Z9P7"}}}, "outcome": [{"profileInstallationResult": {"finalResult": "successResult", "iccid": "984474680000730771F0"}}], "debuginfo": "83680264001370656E64696E674E6F74696669636174696F6E680264001970

Example, invalid resourceId: {"status": "absent"}

#### Deleting orders

The REST API user is responsible to keep the rest table clean. When an order is done or has to be revoked the for
some reason, the API user will use the delete operation to remove the order from the eIM rest table.

Example: http://127.0.0.1:8080/download/delete/8a901bd9-f203-4eae-bcba-12dee32f4444

The eIM will confirm the deletion of the order by responding with a status "deleted".

Example: {"status":"deleted"}

In case the API user fails to delete the order for some reason it will not stay in the database indefinetly. After
some generous timeout, the eIM will automatically delete the order from the REST table. However, the intension behind
this mechanism is to guard against database memory leaks and is not intended to be used as the general mechanism to
get rid of old orders.

#### Listing orders

The REST API user has to keep track of his pending orders. However, there may be circumstances where the API user must
recover his state. In this case it is possible to list out all resourceIds of all orders that are currently in the
rest table of a certain facility.

Example: http://127.0.0.1:8080/download/list

The eIM will respond with a resource_id_list. The API user may then use this list to lookup the state of each order.
Since the list only contains the resourceIds of the current facility the REST API user must repeat the process for
all facilities.

Example: {"resource_id_list": ["2100f52e-83e1-4fed-9d30-f309daf3391a","35074412-654b-4d7b-aec0-e159837b998f","9d6b14df-1875-4827-8236-916383972a19"]}


### tyring out the REST API

Unfortunately the REST API is also a complex interface that is difficult to operate out of the box without any
prior familiarization. To give a system integrator a good starting point the contrib directory contains "tryme-scripts"
that serve as examples and an easy way try out the REST API right away.

There is a tryme script to create downloads (tryme_download.sh) and one tryme script per PSMO/eCO. The scripts are
called with a one letter parameter that refers to a profile that is pre-configured in tryme.cfg (see below). To get
an overview which profiles are available/p reconfigured, the tryme_*.sh script may be called without parameters.

It should be noted that the tryme_*.sh scripts are really just simple examples that can not replace a proper eUICC
management backend. A REST API user must keep track of the orders he submitted, monitor them, check for errors, delete
orders, resubmit orders, etc.

#### tryme.cfg

This is just a simple shellscript that sets some initial variables. Some sample values are already present. However,
in order to make it work with a random eUICC, one must set the EID at the top. In order to be able to create downloads
one must also set the AC (ActivationCode) and if available the ICCID. In case the ICCID is not known it can be obtained
from the result shown by tryme_download.sh.
