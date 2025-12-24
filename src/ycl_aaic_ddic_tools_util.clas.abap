CLASS ycl_aaic_ddic_tools_util DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    METHODS determine_format
      IMPORTING
        i_data_type TYPE string
        i_length    TYPE i OPTIONAL
        i_decimals  TYPE i OPTIONAL
      EXPORTING
        e_o_format  TYPE REF TO cl_xco_ad_built_in_type
        e_error     TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_ddic_tools_util IMPLEMENTATION.

  METHOD determine_format.

    CLEAR e_o_format.

    DATA(l_data_type) = condense( to_upper( i_data_type ) ).

    CASE l_data_type.

      WHEN 'CHAR'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->char( CONV #( i_length ) ).

      WHEN 'INT1'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->int1.

      WHEN 'INT2'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->int2.

      WHEN 'INT4'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->int4.

      WHEN 'DEC'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->dec( iv_length = CONV #( i_length ) iv_decimals = CONV #( i_decimals ) ).

      WHEN 'NUMC'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->numc( CONV #( i_length ) ).

      WHEN 'QUAN'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->quan( iv_length = CONV #( i_length ) iv_decimals = CONV #( i_decimals ) ).

      WHEN 'STRING'.

        IF i_length > 0 AND i_length < 256.
          e_o_format = xco_cp_abap_dictionary=>built_in_type->string( CONV #( 256 ) ).
        ELSE.
          e_o_format = xco_cp_abap_dictionary=>built_in_type->string( CONV #( i_length ) ).
        ENDIF.

      WHEN 'UNIT'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->unit( CONV #( 2 ) ).

      WHEN 'CURR'.

        IF i_length IS INITIAL.
          e_error = |The ABAP built in type { l_data_type } requires a length.|.
          RETURN.
        ENDIF.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->curr( iv_length = CONV #( i_length ) iv_decimals = CONV #( i_decimals ) ).

      WHEN 'CUKY'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->cuky.

      WHEN 'DATS'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->dats.

      WHEN 'TIMS'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->tims.

      WHEN 'FLTP'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->fltp.

      WHEN 'LANG'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->lang.

      WHEN 'CLNT'.

        e_o_format = xco_cp_abap_dictionary=>built_in_type->clnt.

      WHEN OTHERS.

        RETURN.

    ENDCASE.

  ENDMETHOD.
ENDCLASS.
