# The gocix Manual

Edition 0.1  
28 August 2024  

Copyright ©Giacomo Leidi 2024

Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3 or
any later version published by the Free Software Foundation; with no
Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts. A
copy of the license is included in the section entitled “GNU Free
Documentation License”.

------------------------------------------------------------------------

<span id="Top"></span> <span id="gocix"></span>

# gocix

This document describes gocix version 0.1.

|                                 |     |                         |
|:--------------------------------|-----|:------------------------|
| [1 Introduction](#Introduction) |     | Why gocix?              |
| [2 Services](#Services)         |     | Use services from gocix |

------------------------------------------------------------------------

<span id="Introduction"></span> <span id="Introduction-1"></span>

# 1 Introduction

This channel exposes at ` (oci services) ` a set of Guix System services
for many useful applications, such as:

- Bonfire
- Forgejo
- Grafana
- Matrix Conduit
- Meilisearch
- Prometheus
- Prometheus Blackbox Exporter

These services are supposed to feel like services backed by native Guix
packages, please report any inconsistency you may find.

Services in gocix are supposed to be used to run useful stuff with Guix.
They are for the community by the community and there are all the
intentions of collaborating with upstream once underlying packages are
into Guix proper. To achieve this vision gocix services strive to:

- Separate as much as possible configurations that pertain to the OCI
  implementation (i.e. ports, volumes, OCI images tags and so on) from
  application configurations (i.e. whether the application will use
  https or whatever else). The idea is to make it as little effort as
  possible upstreaming code from this channel to Guix.
- Services are configured with sane, secure, privacy aware defaults when
  possible. This means that each service in this repository is carefully
  reviewed and analytics are disabled whenever possible.

------------------------------------------------------------------------

<span id="Services"></span> <span id="Services-1"></span>

# 2 Services

|                                                                   |     |     |
|:------------------------------------------------------------------|-----|:----|
| [2.1 Bonfire](#Bonfire)                                           |     |     |
| [2.2 Conduit](#Conduit)                                           |     |     |
| [2.3 Forgejo](#Forgejo)                                           |     |     |
| [2.4 Grafana](#Grafana)                                           |     |     |
| [2.5 Meilisearch](#Meilisearch)                                   |     |     |
| [2.6 Prometheus](#Prometheus)                                     |     |     |
| [2.7 Prometheus Blackbox Exporter](#Prometheus-Blackbox-Exporter) |     |     |

------------------------------------------------------------------------

<span id="Bonfire"></span> <span id="Bonfire-1"></span>

## 2.1 Bonfire

<span id="index-bonfire_002dconfiguration"></span> Data Type: **bonfire-configuration**  
Available ` bonfire-configuration ` fields are:

` flavour ` (default: ` "classic" ` ) (type: string)  
The flavour of the Bonfire instance. You can refer to [upstream’s
documentation](https://bonfirenetworks.org/docs) for details.

` hostname ` (type: string)  
The domain name where Bonfire will be exposed.

` postgres-host ` (default: ` "localhost" ` ) (type: string)  
The hostname where postgres will be looked for.

` postgres-db ` (default: ` "bonfire_db" ` ) (type: string)  
The database name of the Bonfire’s Postgres database.

` postgres-user ` (default: ` "bonfire" ` ) (type: string)  
The user name that Bonfire will use to authenticate against the Postgres
database.

` mail-server ` (type: maybe-string)  
SMTP domain of the mail server.

` mail-domain ` (type: maybe-string)  
The bit after @ in your email.

` mail-user ` (type: maybe-string)  
The bit before @ in your email.

` mail-from ` (type: maybe-string)  
The email address from which Bonfire will send emails.

` mail-port ` (default: ` "465" ` ) (type: string)  
The port of the SMTP service on your mail server.

` mail-ssl? ` (default: ` #t ` ) (type: boolean)  
Whether to use SSL for the connection to the SMTP server.

` port ` (default: ` "4000" ` ) (type: string)  
The internal port where Bonfire will be exposed.

` public-port ` (default: ` "443" ` ) (type: string)  
The public port where Bonfire will be exposed.

<!-- -->

<span id="index-oci_002dbonfire_002dconfiguration"></span> Data Type: **oci-bonfire-configuration**  
Available ` oci-bonfire-configuration ` fields are:

` image ` (default: ` "bonfirenetworks/bonfire:0.9.10-beta.70-classic-amd64" ` ) (type: string)  
The image to use for the OCI backed Shepherd service.

` upload-data-directory ` (default: ` "/var/lib/bonfire/uploads" ` ) (type: string)  
Upload data directory.

` configuration ` (type: bonfire-configuration)  
A bonfire-configuration record used to configure the Bonfire instance.

` requirement ` (default: ` (postgresql) ` ) (type: list)  
A list of Shepherd services that will be waited for before starting
Bonfire.

` log-file ` (type: maybe-string)  
When ` log-file ` is set, it names the file to which the service’s
standard output and standard error are redirected. ` log-file ` is
created if it does not exist, otherwise it is appended to.

` auto-start? ` (default: ` #t ` ) (type: boolean)  
Whether Bonfire should be started automatically by the Shepherd. If it
is ` #f ` Bonfire has to be started manually with ` herd start ` .

` secrets-directory ` (default: ` "/run/secrets" ` ) (type: string)  
The directory where secrets are looked for.

` secret-key-base ` (type: sops-secret)  
SECRET_KEY_BASE Bonfire secret.

` signing-salt ` (type: sops-secret)  
SIGNING_SALT Bonfire secret.

` encryption-salt ` (type: sops-secret)  
ENCRYPTION_SALT Bonfire secret.

` mail-password ` (type: sops-secret)  
MAIL_KEY Bonfire secret.

` postgres-password ` (type: sops-secret)  
POSTGRES_PASSWORD Bonfire secret.

` meili-master-key ` (type: sops-secret)  
MEILI_MASTER_KEY Bonfire secret.

` network ` (type: maybe-string)  
The docker network where the bonfire container will be attached. When
equal to "host" the ` port ` field will not be mapped into the
container’s one.

` extra-variables ` (default: ` () ` ) (type: list)  
A list of pairs representing any extra environment variable that should
be set inside the container. Refer to the
[https://bonfirenetworks.org/docs/deploy/](mainline) documentation for
more details.

------------------------------------------------------------------------

<span id="Conduit"></span> <span id="Conduit-1"></span>

## 2.2 Conduit

<span id="index-conduit_002dconfiguration"></span> Data Type: **conduit-configuration**  
Available ` conduit-configuration ` fields are:

` image ` (default: ` "matrixconduit/matrix-conduit:v0.6.0" ` ) (type: string)  
The image to use for the OCI backed Shepherd service.

` port ` (default: ` "6167" ` ) (type: string)  
The port where conduit will be exposed.

` server-name ` (type: maybe-string)  
The fully qualified domain name where conduit will be exposed.

` database-path ` (default: ` "/var/lib/matrix-conduit" ` ) (type: string)  
The directory where conduit writes state.

` database-backend ` (default: ` "rocksdb" ` ) (type: string)  
The database backend used by conduit.

` max-request-size ` (default: ` "20000000" ` ) (type: string)  
Expressed in bytes. The default is \~20MB.

` allow-registration? ` (default: ` #f ` ) (type: boolean)  
Whether to allow new users to sign up to the conduit instance.

` allow-federation? ` (default: ` #t ` ) (type: boolean)  
Whether to federate the conduit instance with others in the Matrix
network.

` allow-check-for-updates? ` (default: ` #f ` ) (type: boolean)  
Whether conduit will look for new updates.

` trusted-servers ` (default: ` ("matrix.org") ` ) (type: list-of-strings)  
The list of trusted Matrix servers.

` max-concurrent-requests ` (default: ` "100" ` ) (type: string)  
The maximum number of concurrent requests handled concurrently by
conduit.

` address ` (default: ` "0.0.0.0" ` ) (type: string)  
The ip address where conduit will bind for connections.

` log ` (default: ` "warn,rocket=off,_=off,sled=off" ` ) (type: string)  
The logging configuration for conduit.

------------------------------------------------------------------------

<span id="Forgejo"></span> <span id="Forgejo-1"></span>

## 2.3 Forgejo

<span id="index-forgejo_002dconfiguration"></span> Data Type: **forgejo-configuration**  
Available ` forgejo-configuration ` fields are:

` uid ` (default: ` 34595 ` ) (type: positive)  
The uid assigned to the Forgejo service account.

` gid ` (default: ` 98715 ` ) (type: positive)  
The gid assigned to the Forgejo service account.

` image ` (default: ` "codeberg.org/forgejo/forgejo:1.21.4-0-rootless" ` ) (type: string)  
The image to use for the OCI backed Shepherd service.

` port ` (default: ` "3000" ` ) (type: string)  
The port where forgejo will be exposed.

` ssh-port ` (default: ` "2202" ` ) (type: string)  
The port where forgejo’s ssh service will be exposed.

` datadir ` (default: ` "/var/lib/forgejo" ` ) (type: string)  
The directory where forgejo writes state.

` app.ini ` (type: maybe-file-like)  
The ` app.ini ` configuration passed to Forgejo.

------------------------------------------------------------------------

<span id="Grafana"></span> <span id="Grafana-1"></span>

## 2.4 Grafana

<span id="index-grafana_002dconfiguration"></span> Data Type: **grafana-configuration**  
Available ` grafana-configuration ` fields are:

` server ` (type: grafana-server-configuration)  
grafana.ini’s \[server\] section.

` smtp ` (type: grafana-smtp-configuration)  
grafana.ini’s \[smtp\] section.

` extra-content ` (default: ` "" ` ) (type: string)  
Everything you want to manually add to grafana.ini.

<!-- -->

<span id="index-grafana_002dserver_002dconfiguration"></span> Data Type: **grafana-server-configuration**  
Available ` grafana-server-configuration ` fields are:

` domain ` (default: ` "example.org" ` ) (type: string)  
The public host where grafana will be exposed.

` root-url ` (default: ` "%(protocol)s://%(domain)s:%(http_port)s/" ` ) (type: string)  
The url where grafana will be exposed.

` serve-from-sub-path? ` (default: ` #f ` ) (type: boolean)  
The image to use for the OCI backed Shepherd service.

<!-- -->

<span id="index-grafana_002dsmtp_002dconfiguration"></span> Data Type: **grafana-smtp-configuration**  
Available ` grafana-smtp-configuration ` fields are:

` enabled? ` (default: ` #f ` ) (type: boolean)  
Whether to enable Grafana’s email alerting.

` host ` (default: ` "smtp.example.org:587" ` ) (type: string)  
The connection string representing your SMTP server.

` user ` (default: ` "you@example.org" ` ) (type: string)  
The email used to authenticate with the SMTP server.

` password ` (default: ` "" ` ) (type: string)  
The password used to authenticate with the SMTP server.

` from-address ` (default: ` "alert@example.org" ` ) (type: string)  
The sender of the email alerts Grafana will send.

<!-- -->

<span id="index-oci_002dgrafana_002dconfiguration"></span> Data Type: **oci-grafana-configuration**  
Available ` oci-grafana-configuration ` fields are:

` datadir ` (default: ` "/var/lib/grafana" ` ) (type: string)  
The directory where grafana writes state.

` image ` (default: ` "bitnami/grafana:10.1.5" ` ) (type: string)  
The image to use for the OCI backed Shepherd service.

` port ` (default: ` "3000" ` ) (type: string)  
This host port will be mapped onto the Grafana configured port inside
the container.

` grafana.ini ` (type: grafana-configuration)  
This field will be serialized as graphana.ini.

` network ` (type: maybe-string)  
The docker network where the grafana container will be attached. When
equal to "host" the ` port ` field will be ignored.

------------------------------------------------------------------------

<span id="Meilisearch"></span> <span id="Meilisearch-1"></span>

## 2.5 Meilisearch

<span id="index-oci_002dmeilisearch_002dconfiguration"></span> Data Type: **oci-meilisearch-configuration**  
Available ` oci-meilisearch-configuration ` fields are:

` image ` (default: ` "getmeili/meilisearch:v1.6.0" ` ) (type: string)  
The image to use for the OCI backed Shepherd service.

` port ` (default: ` "7700" ` ) (type: string)  
The port where meilisearch will be exposed.

` master-key ` (type: sops-secret)  
Sets the instance’s master key, automatically protecting all routes
except GET /health. This means you will need a valid API key to access
all other endpoints.

` datadir ` (default: ` "/var/lib/meilisearch/meili_data" ` ) (type: string)  
The directory where meilisearch writes state.

` database-path ` (default: ` "/var/lib/meilisearch/data.ms" ` ) (type: string)  
The directory used by meilisearch database to store state.

` network ` (type: maybe-string)  
The docker network where the meilisearch container will be attached.
When equal to "host" the ` port ` field will not be mapped into the
container’s one.

` extra-variables ` (default: ` () ` ) (type: list)  
A list of pairs representing any extra environment variable that should
be set inside the container. Refer to the
[https://www.meilisearch.com/docs/learn/configuration/instance_options](upstream)
documentation for more details.

------------------------------------------------------------------------

<span id="Prometheus"></span> <span id="Prometheus-1"></span>

## 2.6 Prometheus

<span id="index-oci_002dprometheus_002dconfiguration"></span> Data Type: **oci-prometheus-configuration**  
Available ` oci-prometheus-configuration ` fields are:

` datadir ` (default: ` "/var/lib/prometheus" ` ) (type: string)  
The directory where prometheus writes state.

` file ` (type: maybe-file-like)  
The configuration file to use for the OCI backed Shepherd service.

` record ` (type: maybe-prometheus-configuration)  
The configuration record to use for the OCI backed Shepherd service. If
the ` file ` field is set, this field will be ignored.

` image ` (default: ` "prom/prometheus:v2.45.0" ` ) (type: string)  
The image to use for the OCI backed Shepherd service.

` network ` (type: maybe-string)  
The docker network where the grafana container will be attached. When
equal to "host" the ` port ` field will be ignored.

` port ` (default: ` "9000" ` ) (type: string)  
This host port will be mapped onto the Prometheus dashboard configured
port inside the container.

` metrics-port ` (default: ` "9090" ` ) (type: string)  
This host port will be mapped onto the Prometheus health endpoint
configured port inside the container.

<!-- -->

<span id="index-prometheus_002dconfiguration"></span> Data Type: **prometheus-configuration**  
Available ` prometheus-configuration ` fields are:

` global ` (type: prometheus-global-configuration)  
Prometheus’ ` global ` section.

` scrape-configs ` (default: ` () ` ) (type: list-of-prometheus-scrape-configurations)  
Prometheus’ ` scrape_configs ` section.

` extra-content ` (default: ` "" ` ) (type: string)  
Everything you want to manually append to the configuration file.

<!-- -->

<span id="index-prometheus_002dglobal_002dconfiguration"></span> Data Type: **prometheus-global-configuration**  
Available ` prometheus-global-configuration ` fields are:

` scrape-interval ` (default: ` "30s" ` ) (type: string)  
Prometheus’ ` scrape_interval ` field.

` scrape-timeout ` (default: ` "12s" ` ) (type: string)  
Prometheus’ ` scrape_timeout ` field.

` extra-content ` (default: ` "" ` ) (type: string)  
Everything you want to manually append to the ` global ` section.

<!-- -->

<span id="index-prometheus_002dscrape_002dconfiguration"></span> Data Type: **prometheus-scrape-configuration**  
Available ` prometheus-scrape-configuration ` fields are:

` job-name ` (type: string)  
The name of the scrape job.

` metrics-path ` (default: ` "/metrics" ` ) (type: string)  
The path where this job will scrape metrics.

` static-configs ` (default: ` () ` ) (type: list-of-prometheus-static-configurations)  
The list of static configurations for this job.

` extra-content ` (default: ` "" ` ) (type: string)  
Everything you want to manually append to this ` scrape_config ` field.

<!-- -->

<span id="index-prometheus_002dstatic_002dconfiguration"></span> Data Type: **prometheus-static-configuration**  
Available ` prometheus-static-configuration ` fields are:

` targets ` (default: ` () ` ) (type: list-of-strings)  
The target hosts that will be scraped for metrics.

` extra-content ` (default: ` "" ` ) (type: string)  
Everything you want to manually append to this ` static_config ` field.

------------------------------------------------------------------------

<span id="Prometheus-Blackbox-Exporter"></span> <span
id="Prometheus-Blackbox-Exporter-1"></span>

## 2.7 Prometheus Blackbox Exporter

<span id="index-oci_002dblackbox_002dexporter_002dconfiguration"></span> Data Type: **oci-blackbox-exporter-configuration**  
Available ` oci-blackbox-exporter-configuration ` fields are:

` datadir ` (default: ` "/var/lib/blackbox-exporter" ` ) (type: string)  
The directory where blackbox-exporter writes state.

` file ` (type: file-like)  
The configuration file to use for Blackbox Exporter.

` image ` (default: ` "bitnami/blackbox-exporter:0.24.0" ` ) (type: string)  
The image to use for the OCI backed Shepherd service.

` network ` (type: maybe-string)  
The docker network where the grafana container will be attached. When
equal to "host" the ` port ` field will be ignored.

` port ` (default: ` "9115" ` ) (type: string)  
This host port will be mapped onto the HTTP port inside the container.
If ` network ` is set this field will be ignored.

------------------------------------------------------------------------

<span id="SEC_Contents"></span>

# Table of Contents

<div class="contents">

- <a href="#Introduction" id="toc-Introduction-1">1 Introduction</a>
- <a href="#Services" id="toc-Services-1">2 Services</a>
  - <a href="#Bonfire" id="toc-Bonfire-1">2.1 Bonfire</a>
  - <a href="#Conduit" id="toc-Conduit-1">2.2 Conduit</a>
  - <a href="#Forgejo" id="toc-Forgejo-1">2.3 Forgejo</a>
  - <a href="#Grafana" id="toc-Grafana-1">2.4 Grafana</a>
  - <a href="#Meilisearch" id="toc-Meilisearch-1">2.5 Meilisearch</a>
  - <a href="#Prometheus" id="toc-Prometheus-1">2.6 Prometheus</a>
  - <a href="#Prometheus-Blackbox-Exporter"
    id="toc-Prometheus-Blackbox-Exporter-1">2.7 Prometheus Blackbox
    Exporter</a>

</div>

------------------------------------------------------------------------

This document was generated on *August 28, 2024* using [*texi2html
5.0*](http://www.nongnu.org/texi2html/) .  