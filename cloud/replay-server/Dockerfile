FROM centos:7

ARG USER_ID=14
ARG GROUP_ID=50

RUN yum -y update && yum clean all
RUN yum install -y \
	vsftpd \
	db4-utils \
	db4 && yum clean all

RUN usermod -u ${USER_ID} ftp
RUN groupmod -g ${GROUP_ID} ftp

COPY vsftpd.conf /etc/vsftpd/

COPY run-vsftpd.sh /usr/sbin/
RUN chmod +x /usr/sbin/run-vsftpd.sh
RUN chown -R ftp:ftp /var/ftp

EXPOSE 20 21

ENTRYPOINT ["/usr/sbin/run-vsftpd.sh"]
