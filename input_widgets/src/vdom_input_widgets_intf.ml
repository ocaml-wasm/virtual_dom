open Core

module type Display = sig
  type t

  val to_string : t -> string
end

module type%template [@mode m = (global, local)] Equal = sig
  type t [@@deriving equal [@mode.explicit m]]

  include Display with type t := t
end

module type%template [@mode m = (global, local)] Enum = sig
  type t [@@deriving (equal [@mode.explicit m]), enumerate]

  include Display with type t := t
end

module type Set = sig
  type t
  type comparator_witness

  include Comparator.S with type t := t and type comparator_witness := comparator_witness
  include Display with type t := t
end

module type Enum_set = sig
  type t [@@deriving enumerate]
  type comparator_witness

  include Comparator.S with type t := t and type comparator_witness := comparator_witness
  include Enum with type t := t
end
