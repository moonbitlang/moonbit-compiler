(*
   Copyright (C) 2024 International Digital Economy Academy.
   This program is licensed under the MoonBit Public Source
   License as published by the International Digital Economy Academy,
   either version 1 of the License, or (at your option) any later
   version. This program is distributed in the hope that it will be
   useful, but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the MoonBit
   Public Source License for more details. You should have received a
   copy of the MoonBit Public Source License along with this program. If
   not, see
   <https://www.moonbitlang.com/licenses/moonbit-public-source-license-v1>.
*)


let runtime_gc : Dwarfsm_ast.module_ =
  {
    id = { id = None; index = -1 };
    fields =
      [
        Import
          {
            module_ = "spectest";
            name = "print_i32";
            desc =
              Func
                ( { id = Some "$print_i32"; index = -1 },
                  Inline
                    ( [
                        {
                          id = { id = Some "$i"; index = -1 };
                          source_name = None;
                          type_ = Numtype I32;
                          source_type = None;
                        };
                      ],
                      [] ) );
          };
        Import
          {
            module_ = "spectest";
            name = "print_i64";
            desc =
              Func
                ( { id = Some "$print_i64"; index = -1 },
                  Inline
                    ( [
                        {
                          id = { id = Some "$i"; index = -1 };
                          source_name = None;
                          type_ = Numtype I64;
                          source_type = None;
                        };
                      ],
                      [] ) );
          };
        Import
          {
            module_ = "spectest";
            name = "print_char";
            desc =
              Func
                ( { id = Some "$printc"; index = -1 },
                  Inline
                    ( [
                        {
                          id = { id = Some "$i"; index = -1 };
                          source_name = None;
                          type_ = Numtype I32;
                          source_type = None;
                        };
                      ],
                      [] ) );
          };
        Import
          {
            module_ = "spectest";
            name = "print_f64";
            desc =
              Func
                ( { id = Some "$print_f64"; index = -1 },
                  Inline
                    ( [
                        {
                          id = { id = Some "$i"; index = -1 };
                          source_name = None;
                          type_ = Numtype F64;
                          source_type = None;
                        };
                      ],
                      [] ) );
          };
        Rectype
          [
            ( { id = Some "$moonbit.enum"; index = -1 },
              {
                final = false;
                super = [];
                type_ =
                  Structtype
                    (Struct
                       [
                         {
                           id = { id = None; index = -1 };
                           fieldtype =
                             { mut = Const; type_ = Valtype (Numtype I32) };
                         };
                       ]);
              } );
          ];
        Rectype
          [
            ( { id = Some "$moonbit.string"; index = -1 },
              {
                final = true;
                super = [];
                type_ = Arraytype (Array { mut = Var; type_ = Packedtype I16 });
              } );
          ];
        Rectype
          [
            ( { id = Some "$moonbit.bytes"; index = -1 },
              {
                final = true;
                super = [];
                type_ = Arraytype (Array { mut = Var; type_ = Packedtype I8 });
              } );
          ];
        Rectype
          [
            ( { id = Some "$moonbit.open_empty_struct"; index = -1 },
              { final = false; super = []; type_ = Structtype (Struct []) } );
          ];
        Rectype
          [
            ( { id = Some "$moonbit.string_pool_type"; index = -1 },
              {
                final = true;
                super = [];
                type_ =
                  Arraytype
                    (Array
                       {
                         mut = Var;
                         type_ =
                           Valtype
                             (Reftype
                                (Ref
                                   ( Nullable,
                                     Type { var = Unresolve "$moonbit.string" }
                                   )));
                       });
              } );
          ];
        Func
          {
            id = { id = Some "$moonbit.string_equal"; index = -1 };
            type_ =
              Inline
                ( [
                    {
                      id = { id = Some "$stra"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             ( NonNull,
                               Type { var = Unresolve "$moonbit.string" } ));
                      source_type = None;
                    };
                    {
                      id = { id = Some "$strb"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             ( NonNull,
                               Type { var = Unresolve "$moonbit.string" } ));
                      source_type = None;
                    };
                  ],
                  [ Numtype I32 ] );
            locals =
              [
                {
                  id = { id = Some "$counter"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
                {
                  id = { id = Some "$stra_len"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
              ];
            code =
              [
                Local_get { var = Unresolve "$stra" };
                Array_len;
                Local_set { var = Unresolve "$stra_len" };
                Local_get { var = Unresolve "$stra_len" };
                Local_get { var = Unresolve "$strb" };
                Array_len;
                I32_eq;
                If
                  {
                    label = None;
                    typeuse = Inline ([], [ Numtype I32 ]);
                    then_ =
                      [
                        Loop
                          {
                            label = Some "$loop";
                            typeuse = Inline ([], [ Numtype I32 ]);
                            instrs =
                              [
                                Local_get { var = Unresolve "$counter" };
                                Local_get { var = Unresolve "$stra_len" };
                                I32_lt_s;
                                If
                                  {
                                    label = None;
                                    typeuse = Inline ([], [ Numtype I32 ]);
                                    then_ =
                                      [
                                        Local_get { var = Unresolve "$stra" };
                                        Local_get { var = Unresolve "$counter" };
                                        Array_get_u
                                          { var = Unresolve "$moonbit.string" };
                                        Local_get { var = Unresolve "$strb" };
                                        Local_get { var = Unresolve "$counter" };
                                        Array_get_u
                                          { var = Unresolve "$moonbit.string" };
                                        I32_eq;
                                        If
                                          {
                                            label = None;
                                            typeuse =
                                              Inline ([], [ Numtype I32 ]);
                                            then_ =
                                              [
                                                Local_get
                                                  { var = Unresolve "$counter" };
                                                I32_const 1l;
                                                I32_add;
                                                Local_set
                                                  { var = Unresolve "$counter" };
                                                Br { var = Unresolve "$loop" };
                                              ];
                                            else_ = [ I32_const 0l; Return ];
                                          };
                                      ];
                                    else_ = [ I32_const 1l; Return ];
                                  };
                              ];
                          };
                      ];
                    else_ = [ I32_const 0l; Return ];
                  };
              ];
            source_name = None;
            aux = { low_pc = 0; high_pc = 0 };
          };
        Func
          {
            id = { id = Some "$moonbit.bytes_equal"; index = -1 };
            type_ =
              Inline
                ( [
                    {
                      id = { id = Some "$lhs"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             (NonNull, Type { var = Unresolve "$moonbit.bytes" }));
                      source_type = None;
                    };
                    {
                      id = { id = Some "$rhs"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             (NonNull, Type { var = Unresolve "$moonbit.bytes" }));
                      source_type = None;
                    };
                  ],
                  [ Numtype I32 ] );
            locals =
              [
                {
                  id = { id = Some "$counter"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
                {
                  id = { id = Some "$len"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
              ];
            code =
              [
                Local_get { var = Unresolve "$lhs" };
                Array_len;
                Local_set { var = Unresolve "$len" };
                Local_get { var = Unresolve "$len" };
                Local_get { var = Unresolve "$rhs" };
                Array_len;
                I32_eq;
                If
                  {
                    label = None;
                    typeuse = Inline ([], [ Numtype I32 ]);
                    then_ =
                      [
                        Loop
                          {
                            label = Some "$loop";
                            typeuse = Inline ([], [ Numtype I32 ]);
                            instrs =
                              [
                                Local_get { var = Unresolve "$counter" };
                                Local_get { var = Unresolve "$len" };
                                I32_lt_s;
                                If
                                  {
                                    label = None;
                                    typeuse = Inline ([], [ Numtype I32 ]);
                                    then_ =
                                      [
                                        Local_get { var = Unresolve "$lhs" };
                                        Local_get { var = Unresolve "$counter" };
                                        Array_get_u
                                          { var = Unresolve "$moonbit.bytes" };
                                        Local_get { var = Unresolve "$rhs" };
                                        Local_get { var = Unresolve "$counter" };
                                        Array_get_u
                                          { var = Unresolve "$moonbit.bytes" };
                                        I32_eq;
                                        If
                                          {
                                            label = None;
                                            typeuse =
                                              Inline ([], [ Numtype I32 ]);
                                            then_ =
                                              [
                                                Local_get
                                                  { var = Unresolve "$counter" };
                                                I32_const 1l;
                                                I32_add;
                                                Local_set
                                                  { var = Unresolve "$counter" };
                                                Br { var = Unresolve "$loop" };
                                              ];
                                            else_ = [ I32_const 0l; Return ];
                                          };
                                      ];
                                    else_ = [ I32_const 1l; Return ];
                                  };
                              ];
                          };
                      ];
                    else_ = [ I32_const 0l ];
                  };
              ];
            source_name = None;
            aux = { low_pc = 0; high_pc = 0 };
          };
        Func
          {
            id = { id = Some "$moonbit.println"; index = -1 };
            type_ =
              Inline
                ( [
                    {
                      id = { id = Some "$str"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             ( NonNull,
                               Type { var = Unresolve "$moonbit.string" } ));
                      source_type = None;
                    };
                  ],
                  [] );
            locals =
              [
                {
                  id = { id = Some "$counter"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
              ];
            code =
              [
                Loop
                  {
                    label = Some "$loop";
                    typeuse = Inline ([], []);
                    instrs =
                      [
                        Local_get { var = Unresolve "$counter" };
                        Local_get { var = Unresolve "$str" };
                        Array_len;
                        I32_lt_s;
                        If
                          {
                            label = None;
                            typeuse = Inline ([], []);
                            then_ =
                              [
                                Local_get { var = Unresolve "$str" };
                                Local_get { var = Unresolve "$counter" };
                                Array_get_u
                                  { var = Unresolve "$moonbit.string" };
                                Call { var = Unresolve "$printc" };
                                Local_get { var = Unresolve "$counter" };
                                I32_const 1l;
                                I32_add;
                                Local_set { var = Unresolve "$counter" };
                                Br { var = Unresolve "$loop" };
                              ];
                            else_ = [];
                          };
                      ];
                  };
                I32_const 10l;
                Call { var = Unresolve "$printc" };
              ];
            source_name = None;
            aux = { low_pc = 0; high_pc = 0 };
          };
        Func
          {
            id = { id = Some "$moonbit.unsafe_make_string"; index = -1 };
            type_ =
              Inline
                ( [
                    {
                      id = { id = Some "$len"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                    {
                      id = { id = Some "$val"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                  ],
                  [
                    Reftype
                      (Ref (NonNull, Type { var = Unresolve "$moonbit.string" }));
                  ] );
            locals = [];
            code =
              [
                Local_get { var = Unresolve "$val" };
                Local_get { var = Unresolve "$len" };
                Array_new { var = Unresolve "$moonbit.string" };
              ];
            source_name = None;
            aux = { low_pc = 0; high_pc = 0 };
          };
        Func
          {
            id = { id = Some "$moonbit.add_string"; index = -1 };
            type_ =
              Inline
                ( [
                    {
                      id = { id = Some "$x"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             ( NonNull,
                               Type { var = Unresolve "$moonbit.string" } ));
                      source_type = None;
                    };
                    {
                      id = { id = Some "$y"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             ( NonNull,
                               Type { var = Unresolve "$moonbit.string" } ));
                      source_type = None;
                    };
                  ],
                  [
                    Reftype
                      (Ref (NonNull, Type { var = Unresolve "$moonbit.string" }));
                  ] );
            locals =
              [
                {
                  id = { id = Some "$lenx"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
                {
                  id = { id = Some "$leny"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
                {
                  id = { id = Some "$len"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
                {
                  id = { id = Some "$ptr"; index = -1 };
                  source_name = None;
                  type_ =
                    Reftype
                      (Ref (NonNull, Type { var = Unresolve "$moonbit.string" }));
                  source_type = None;
                };
              ];
            code =
              [
                Local_get { var = Unresolve "$x" };
                Array_len;
                Local_set { var = Unresolve "$lenx" };
                Local_get { var = Unresolve "$y" };
                Array_len;
                Local_set { var = Unresolve "$leny" };
                Local_get { var = Unresolve "$lenx" };
                Local_get { var = Unresolve "$leny" };
                I32_add;
                Local_set { var = Unresolve "$len" };
                Local_get { var = Unresolve "$len" };
                Array_new_default { var = Unresolve "$moonbit.string" };
                Local_set { var = Unresolve "$ptr" };
                Local_get { var = Unresolve "$ptr" };
                I32_const 0l;
                Local_get { var = Unresolve "$x" };
                I32_const 0l;
                Local_get { var = Unresolve "$lenx" };
                Array_copy
                  ( { var = Unresolve "$moonbit.string" },
                    { var = Unresolve "$moonbit.string" } );
                Local_get { var = Unresolve "$ptr" };
                Local_get { var = Unresolve "$lenx" };
                Local_get { var = Unresolve "$y" };
                I32_const 0l;
                Local_get { var = Unresolve "$leny" };
                Array_copy
                  ( { var = Unresolve "$moonbit.string" },
                    { var = Unresolve "$moonbit.string" } );
                Local_get { var = Unresolve "$ptr" };
              ];
            source_name = None;
            aux = { low_pc = 0; high_pc = 0 };
          };
        Func
          {
            id = { id = Some "$moonbit.string_literal"; index = -1 };
            type_ =
              Inline
                ( [
                    {
                      id = { id = Some "$index"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                    {
                      id = { id = Some "$offset"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                    {
                      id = { id = Some "$length"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                  ],
                  [
                    Reftype
                      (Ref (NonNull, Type { var = Unresolve "$moonbit.string" }));
                  ] );
            locals =
              [
                {
                  id = { id = Some "$cached"; index = -1 };
                  source_name = None;
                  type_ =
                    Reftype
                      (Ref (Nullable, Type { var = Unresolve "$moonbit.string" }));
                  source_type = None;
                };
                {
                  id = { id = Some "$new_string"; index = -1 };
                  source_name = None;
                  type_ =
                    Reftype
                      (Ref (NonNull, Type { var = Unresolve "$moonbit.string" }));
                  source_type = None;
                };
              ];
            code =
              [
                Global_get { var = Unresolve "$moonbit.string_pool" };
                Local_get { var = Unresolve "$index" };
                Array_get { var = Unresolve "$moonbit.string_pool_type" };
                Local_tee { var = Unresolve "$cached" };
                Ref_is_null;
                I32_eqz;
                If
                  {
                    label = None;
                    typeuse = Inline ([], []);
                    then_ =
                      [
                        Local_get { var = Unresolve "$cached" };
                        Ref_as_non_null;
                        Return;
                      ];
                    else_ = [];
                  };
                Local_get { var = Unresolve "$offset" };
                Local_get { var = Unresolve "$length" };
                Array_new_data
                  ( { var = Unresolve "$moonbit.string" },
                    { var = Unresolve "$moonbit.const_data" } );
                Local_set { var = Unresolve "$new_string" };
                Global_get { var = Unresolve "$moonbit.string_pool" };
                Local_get { var = Unresolve "$index" };
                Local_get { var = Unresolve "$new_string" };
                Array_set { var = Unresolve "$moonbit.string_pool_type" };
                Local_get { var = Unresolve "$new_string" };
              ];
            source_name = None;
            aux = { low_pc = 0; high_pc = 0 };
          };
        Func
          {
            id = { id = Some "$moonbit.unsafe_bytes_blit"; index = -1 };
            type_ =
              Inline
                ( [
                    {
                      id = { id = Some "$dst"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             (NonNull, Type { var = Unresolve "$moonbit.bytes" }));
                      source_type = None;
                    };
                    {
                      id = { id = Some "$dst_offset"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                    {
                      id = { id = Some "$src"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             (NonNull, Type { var = Unresolve "$moonbit.bytes" }));
                      source_type = None;
                    };
                    {
                      id = { id = Some "$src_offset"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                    {
                      id = { id = Some "$length"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                  ],
                  [] );
            locals = [];
            code =
              [
                Local_get { var = Unresolve "$dst" };
                Local_get { var = Unresolve "$dst_offset" };
                Local_get { var = Unresolve "$src" };
                Local_get { var = Unresolve "$src_offset" };
                Local_get { var = Unresolve "$length" };
                Array_copy
                  ( { var = Unresolve "$moonbit.bytes" },
                    { var = Unresolve "$moonbit.bytes" } );
              ];
            source_name = None;
            aux = { low_pc = 0; high_pc = 0 };
          };
        Func
          {
            id = { id = Some "$moonbit.unsafe_bytes_sub_string"; index = -1 };
            type_ =
              Inline
                ( [
                    {
                      id = { id = Some "$src"; index = -1 };
                      source_name = None;
                      type_ =
                        Reftype
                          (Ref
                             (NonNull, Type { var = Unresolve "$moonbit.bytes" }));
                      source_type = None;
                    };
                    {
                      id = { id = Some "$offset"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                    {
                      id = { id = Some "$length"; index = -1 };
                      source_name = None;
                      type_ = Numtype I32;
                      source_type = None;
                    };
                  ],
                  [
                    Reftype
                      (Ref (NonNull, Type { var = Unresolve "$moonbit.string" }));
                  ] );
            locals =
              [
                {
                  id = { id = Some "$dst"; index = -1 };
                  source_name = None;
                  type_ =
                    Reftype
                      (Ref (NonNull, Type { var = Unresolve "$moonbit.string" }));
                  source_type = None;
                };
                {
                  id = { id = Some "$strlen"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
                {
                  id = { id = Some "$ch"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
                {
                  id = { id = Some "$i"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
                {
                  id = { id = Some "$j"; index = -1 };
                  source_name = None;
                  type_ = Numtype I32;
                  source_type = None;
                };
              ];
            code =
              [
                Local_get { var = Unresolve "$length" };
                I32_const 1l;
                I32_shr_s;
                Local_set { var = Unresolve "$strlen" };
                I32_const 0l;
                Local_get { var = Unresolve "$strlen" };
                Array_new { var = Unresolve "$moonbit.string" };
                Local_set { var = Unresolve "$dst" };
                Loop
                  {
                    label = Some "$loop";
                    typeuse = Inline ([], []);
                    instrs =
                      [
                        Local_get { var = Unresolve "$i" };
                        Local_get { var = Unresolve "$strlen" };
                        I32_lt_s;
                        If
                          {
                            label = None;
                            typeuse = Inline ([], []);
                            then_ =
                              [
                                Local_get { var = Unresolve "$offset" };
                                Local_get { var = Unresolve "$i" };
                                I32_const 1l;
                                I32_shl;
                                I32_add;
                                Local_set { var = Unresolve "$j" };
                                Local_get { var = Unresolve "$src" };
                                Local_get { var = Unresolve "$j" };
                                Array_get_u { var = Unresolve "$moonbit.bytes" };
                                Local_get { var = Unresolve "$src" };
                                Local_get { var = Unresolve "$j" };
                                I32_const 1l;
                                I32_add;
                                Array_get_u { var = Unresolve "$moonbit.bytes" };
                                I32_const 8l;
                                I32_shl;
                                I32_or;
                                Local_set { var = Unresolve "$ch" };
                                Local_get { var = Unresolve "$dst" };
                                Local_get { var = Unresolve "$i" };
                                Local_get { var = Unresolve "$ch" };
                                Array_set { var = Unresolve "$moonbit.string" };
                                Local_get { var = Unresolve "$i" };
                                I32_const 1l;
                                I32_add;
                                Local_set { var = Unresolve "$i" };
                                Br { var = Unresolve "$loop" };
                              ];
                            else_ = [];
                          };
                      ];
                  };
                Local_get { var = Unresolve "$dst" };
              ];
            source_name = None;
            aux = { low_pc = 0; high_pc = 0 };
          };
      ];
  }
