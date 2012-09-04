# Glacier Proxy

The Amazon Glacier API is quite complex and can only be utilized with custom
code. Glacier proxy will take care of all the nitty-gritty details of the
Amazon API internally and expose it as a simple to use REST API.

While encapsulating a lot of complexity, Glacier Proxy can still be queried for
the status of all ongoing activity.

The goal of Glacier Proxy is to enable scripted uploads, particularly of large
streams of data that do not exist on disk. It acts as a single point of contact
to initiate new jobs and to keep track of the progress/state of the tasks
submitted to it.


## REST API

The CURL examples assume a Glacier Proxy to be running on the same machine on
its default port of 8001.


### Query Status

    GET /status

#### JSON Response

    {"jobs":[{"jobId":"example1",
              "command":"upload",
              "bytesDone:1234",
              "bytesTotal":4321,  // when the request sent Content-Length header
              "status":"active",
              "msg":"extended human readable message for the status"},
             ...]}


### Upload New Archive

    POST /vault/:vault_name?job_id=:user_supplied_unique_reference

#### JSON Response

    {"sha1":"f572d396fae9206628714fb2ce00f72e94f2258f",
     "date:"Sun, 2 Sep 2012 12:00:00 GMT",
     "archiveId":"EXAMPLEArchiveId",
     "location":"/:aws_user_id/vaults/myvault/archives/EXAMPLEArchiveId"}

#### Usage Example: Upload an Existing File

    curl -X POST --upload-file mypreciousfile \
      http://localhost:8001/vault/myvault?job_id=example1

#### Usage Example: Upload a Data Stream

In this example we create a data stream as Stdin to CURL that represents
a compressed and encrypted version of `mypreciousfile`.

    gzip myfile | \
      openssl enc -aes-256-cbc -salt -pass pass:secretkey | \
      curl -X POST --upload-file - \
        http://localhost:8001/vault/myvault?job_id=example2

To be able to verify the content `sha1` in the proxy response, we can caclulate
a SHA1 on the fly by making use of Bash process substitution like this:

    gzip myfile | \
      openssl enc -aes-256-cbc -salt -pass pass:secretkey | \
      tee >(shasum > shasum.txt) | \
      curl -X POST --upload-file - \
        http://localhost:8001/vault/myvault?job_id=example3

Afterwards we can compare the SHA in `shasum.txt` to the one in the JSON
response to make absolutely sure that the file has been uploaded correctly.
