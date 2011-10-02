
let rec really_read fd string off n =
  if n=0 then () else
    let m = Unix.read fd string off n in
    if m = 0 then raise End_of_file;
    really_read fd string (off+m) (n-m)

let really_read_string fd length =
  let buf = String.make length '\000' in
  really_read fd buf 0 length;
  buf

module Message = struct
  type t = {
    control: bool;
    version: int;
    ty: int;
    flags: int;
    length: int;
    data: Bitstring.t;
  }
  let unmarshal fd =
    (* The first 8 bytes contain the length of the message *)
    let hdr = really_read_string fd 8 in
    let bs = Bitstring.bitstring_of_string hdr in
    bitmatch bs with
      | { control: 1;
	  version: 15: int;
	  ty: 16: int;
	  flags: 8: int;
	  length: 24: int } ->
	let data = really_read_string fd length in
	let data = Bitstring.bitstring_of_string data in {
	control = control; version = version; ty = ty;
	flags = flags; length = length; data = data
      }
      | { _ } -> failwith "Failed to parse header"

end

let take num one bits =
  let rec loop acc bits = function
    | 0l -> List.rev acc
    | n ->
      let nv, rest = one bits in
      loop (nv :: acc) rest (Int32.sub n 1l) in
  loop [] bits num

module NVPairs = struct
  let unmarshal bits =
    let num, rest = bitmatch bits with
      | { num: 16;
	  rest: -1: bitstring
	} -> Int32.of_int num, rest
      | { _ } -> failwith "Failed to parse NVPairs len" in
    let one bits = bitmatch bits with
      | { name_len: 16;
	  name: name_len * 8: string;
	  v_len: 16;
	  v: v_len * 8: string;
	  rest: -1: bitstring
	} -> (name, v), rest
      | { _ } -> failwith "Failed to parse NVPair" in
    take num one rest
end

module IdVPairs = struct
  let unmarshal bits =
    let num, rest = bitmatch bits with
      | { num: 32;
	  rest: -1: bitstring
	} -> num, rest in
    let one bits = bitmatch bits with
      | { id: 24;
	  flags: 8;
	  v: 32
	} -> (id, flags, v), rest
      | { _ } -> failwith "Failed to parse NVPair" in
    take num one rest
end

module Control = struct
  module Syn = struct
    type flag =
      | Fin
      | Unidirectional
    let flags = [
      0x01, Fin;
      0x02, Unidirectional
    ]
    type t = {
      stream_id: int; (* 31 bits *)
      associated_to_stream_id: int; (* 31 bits *)
      pri: int; (* 2 bits *)
      flags: flag list;
      headers: (string * string) list;
    }
    let unmarshal (x: Message.t) =
      let flags = List.map snd (List.filter (fun (mask, flag) -> x.Message.flags land mask <> 0) flags) in
      bitmatch x.Message.data with
	| { _: 1;
	    stream_id: 31;
	    _: 1;
	    associated_to_stream_id: 31;
	    pri: 2;
	    _: 14;
	    rest: -1: bitstring
	  } ->
	  let headers = NVPairs.unmarshal rest in {
	    stream_id = stream_id;
	    associated_to_stream_id = associated_to_stream_id;
	    pri = pri;
	    flags = flags;
	    headers = headers
	  }
	| { _ } -> failwith "Failed to parse SYN"
  end
  module Reply = struct
    type flag =
      | Fin
    type t = {
      stream_id: int; (* 31 bits *)
      flags: flag list;
      headers: (string * string) list;
    }
  end
  module Rst = struct
    type status_code = 
      | Protocol_error
      | Invalid_stream
      | Refused_stream
      | Unsupported_version
      | Cancel
      | Internal_error
      | Flow_control_error
    let status_codes = [
      1l, Protocol_error;
      2l, Invalid_stream;
      3l, Refused_stream;
      4l, Unsupported_version;
      5l, Cancel;
      6l, Internal_error;
      7l, Flow_control_error
    ]
    type t = {
      stream_id: int; (* 31 bits *)
      status_code: status_code;
    }
    let unmarshal (x: Message.t) =
      bitmatch x.Message.data with
	| { _: 1;
	    stream_id: 31;
	    status_code: 32
	  } ->
	  if not (List.mem_assoc status_code status_codes)
	  then failwith (Printf.sprintf "Unknown RST status code: %ld" status_code);
	  { stream_id = stream_id;
	    status_code = List.assoc status_code status_codes }
	| { _ } -> failwith "Failed to parse RST"
  end
  module Settings = struct
    type flag =
      | Clear_previously_persisted_settings
    let flags = [
      0x1, Clear_previously_persisted_settings
    ]
    type id_flag =
      | Persist_value
      | Persisted
    let id_flags = [
      0x1, Persist_value;
      0x2, Persisted;
    ]
    type id =
      | Upload_bandwidth
      | Download_bandwidth
      | Round_trip_time
      | Max_concurrent_streams
      | Current_cwnd
      | Download_retrans_rate
      | Initial_window_size
    let ids = [
      1, Upload_bandwidth;
      2, Download_bandwidth;
      3, Round_trip_time;
      4, Max_concurrent_streams;
      5, Current_cwnd;
      6, Download_retrans_rate;
      7, Initial_window_size;
    ]
    type t = {
      flags: flag list;
      settings: (id * id_flag * Int32.t) list;
    }
    let unmarshal (x: Message.t) =
      let flags = List.map snd (List.filter (fun (mask, flag) -> x.Message.flags land mask <> 0) flags) in
      let raw_settings = IdVPairs.unmarshal x.Message.data in
      let settings =
	List.map (fun (id, id_flag, v) ->
	  if not(List.mem_assoc id ids)
	  then failwith (Printf.sprintf "Unknown SETTINGS id %d" id);
	  if not(List.mem_assoc id_flag id_flags)
	  then failwith (Printf.sprintf "Unknown SETTINGS id_flag %d" id_flag);
	  List.assoc id ids, List.assoc id_flag id_flags, v
	) raw_settings in
      { flags = flags;
	settings = settings }
  end
  module Noop = struct
    type t = unit
    let unmarshal (x: Message.t) = ()
  end
  module Ping = struct
    type t = int32
    let unmarshal (x: Message.t) =
      bitmatch x.Message.data with
	| { id: 32 } -> id
	| { _ } -> failwith "Failed to parse PING"
  end
  module Goaway = struct
    type t = {
      last_good_stream_id: int; (* 31 bits *)
    }
    let unmarshal (x: Message.t) =
      bitmatch x.Message.data with
	| { _: 1;
	    last_good_stream_id: 31 } -> {
	  last_good_stream_id = last_good_stream_id
	}
	| { _ } -> failwith "Failed to parse GOAWAY"
  end
  module Headers = struct
    type t = {
      stream_id: int; (* 31 bits *)
      headers: (string * string) list;
    }
  end
  type t =
    | Syn of Syn.t
    | Reply of Reply.t
    | Rst of Rst.t
    | Settings of Settings.t
    | Noop of Noop.t
    | Ping of Ping.t
    | Goaway of Goaway.t
    | Headers of Headers.t
end



