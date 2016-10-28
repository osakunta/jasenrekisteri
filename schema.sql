-- create schema jasen2;

create table credentials (
  username text not null primary key,
  password text not null
);

create sequence event_id_seq;

create table events (
  eid int not null default nextval('event_id_seq') primary key,
  username text not null, -- username adder
  updated timestamp with time zone not null default current_timestamp,
  edata text not null
);
