CLASS ycl_aaic_class_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    CONSTANTS: mc_public  TYPE string VALUE 'PUBLIC',
               mc_private TYPE string VALUE 'PRIVATE'.

    METHODS create
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_description       TYPE yde_aaic_fc_description
                i_interface_name    TYPE yde_aaic_fc_interface_name OPTIONAL
                i_superclass_name   TYPE yde_aaic_fc_superclass_name OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_package           TYPE yde_aaic_fc_package
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_method
      IMPORTING
                i_class_name         TYPE yde_aaic_class_name
                i_method_name        TYPE yde_aaic_method_name
                i_description        TYPE yde_aaic_fc_description
                i_class_section      TYPE yde_aaic_fc_clas_section DEFAULT mc_public
                i_static             TYPE yde_aaic_fc_static_method DEFAULT abap_false
                i_source             TYPE string
                i_transport_request  TYPE yde_aaic_fc_transport_request OPTIONAL
                i_s_returning_param  TYPE yst_aaic_fc_method_return_par OPTIONAL
                i_t_importing_params TYPE ytt_aaic_fc_method_imp_par OPTIONAL
                i_t_exporting_params TYPE ytt_aaic_fc_method_exp_par OPTIONAL
                i_t_changing_params  TYPE ytt_aaic_fc_method_chang_par OPTIONAL
                i_t_exceptions       TYPE ytt_aaic_fc_method_exceptions OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS change_method_implementation
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_method_name       TYPE yde_aaic_method_name
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
                i_source            TYPE string
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_method_parameters
      IMPORTING
                i_class_name         TYPE yde_aaic_class_name
                i_method_name        TYPE yde_aaic_method_name
                i_transport_request  TYPE yde_aaic_fc_transport_request OPTIONAL
                i_s_returning_param  TYPE yst_aaic_fc_method_return_par OPTIONAL
                i_t_importing_params TYPE ytt_aaic_fc_method_imp_par OPTIONAL
                i_t_exporting_params TYPE ytt_aaic_fc_method_exp_par OPTIONAL
                i_t_changing_params  TYPE ytt_aaic_fc_method_chang_par OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS delete_method_parameters
      IMPORTING
                i_class_name         TYPE yde_aaic_class_name
                i_method_name        TYPE yde_aaic_method_name
                i_transport_request  TYPE yde_aaic_fc_transport_request OPTIONAL
                i_s_returning_param  TYPE yst_aaic_fc_parameter OPTIONAL
                i_t_importing_params TYPE ytt_aaic_fc_parameter OPTIONAL
                i_t_exporting_params TYPE ytt_aaic_fc_parameter OPTIONAL
                i_t_changing_params  TYPE ytt_aaic_fc_parameter OPTIONAL
      RETURNING VALUE(r_response)    TYPE string.

    METHODS delete_method
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_method_name       TYPE yde_aaic_method_name
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_attribute
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_attribute_name    TYPE yde_aaic_fc_clas_attr_name
                i_attribute_type    TYPE yde_aaic_fc_clas_attr_type
                i_class_section     TYPE yde_aaic_fc_clas_section DEFAULT mc_public
                i_static            TYPE yde_aaic_fc_static_method DEFAULT abap_false
                i_read_only         TYPE abap_boolean DEFAULT abap_true
                i_default_value     TYPE string OPTIONAL
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

    METHODS add_constant
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_constant_name     TYPE yde_aaic_fc_constant_name
                i_constant_type     TYPE yde_aaic_fc_constant_type
                i_constant_value    TYPE yde_aaic_fc_constant_value
                i_class_section     TYPE yde_aaic_fc_clas_section DEFAULT mc_public
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

    METHODS delete_attribute
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_attribute_name    TYPE yde_aaic_fc_clas_attr_name
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

    METHODS delete_constant
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_constant_name     TYPE yde_aaic_fc_constant_name
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

    METHODS get_class_definition
      IMPORTING
                i_class_name      TYPE yde_aaic_class_name
      RETURNING VALUE(r_response) TYPE string.

    METHODS get_method_definition
      IMPORTING
                i_class_name      TYPE yde_aaic_class_name
                i_method_name     TYPE yde_aaic_method_name
      RETURNING VALUE(r_response) TYPE string.

    METHODS get_method_implementation
      IMPORTING
                i_class_name      TYPE yde_aaic_class_name
                i_method_name     TYPE yde_aaic_method_name
      RETURNING VALUE(r_response) TYPE string.

    METHODS get_translation
      IMPORTING
                i_class_name      TYPE yde_aaic_class_name
                i_text_symbol_id  TYPE string
                i_language        TYPE spras
      RETURNING VALUE(r_response) TYPE string.

    METHODS set_translation
      IMPORTING
                i_class_name        TYPE yde_aaic_class_name
                i_text_symbol_id    TYPE string
                i_language          TYPE spras
                i_text              TYPE string
                i_transport_request TYPE yde_aaic_fc_transport_request OPTIONAL
      RETURNING VALUE(r_response)   TYPE string.

  PROTECTED SECTION.

  PRIVATE SECTION.

    METHODS _add_findings_to_response
      IMPORTING
                i_o_findings TYPE REF TO if_xco_gen_o_f_section
      CHANGING  ch_response  TYPE csequence.

    METHODS _get_attribute_section
      IMPORTING
        i_class_name     TYPE yde_aaic_class_name
        i_attribute_name TYPE yde_aaic_method_name
      EXPORTING
        e_section        TYPE string
        e_static         TYPE abap_bool.

    METHODS _get_method_section
      IMPORTING
        i_class_name  TYPE yde_aaic_class_name
        i_method_name TYPE yde_aaic_method_name
      EXPORTING
        e_section     TYPE string
        e_static      TYPE abap_bool.

    METHODS _get_transport_request
      IMPORTING
                i_class_name               TYPE yde_aaic_class_name
      RETURNING VALUE(r_transport_request) TYPE sxco_transport.

    METHODS _get_package
      IMPORTING
                i_class_name     TYPE yde_aaic_class_name
      RETURNING VALUE(r_package) TYPE sxco_package.

    METHODS _get_attributes
      IMPORTING
        io_class                TYPE REF TO if_xco_ao_class
      EXPORTING
        e_t_class_datas_public  TYPE sxco_t_ao_c_class_datas
        e_t_datas_public        TYPE sxco_t_ao_c_datas
        e_t_class_datas_private TYPE sxco_t_ao_c_class_datas
        e_t_datas_private       TYPE sxco_t_ao_c_datas.

    METHODS _get_methods
      IMPORTING
        io_class                  TYPE REF TO if_xco_ao_class
      EXPORTING
        e_t_class_methods_public  TYPE sxco_t_clas_c_methods
        e_t_methods_public        TYPE sxco_t_clas_c_methods
        e_t_class_methods_private TYPE sxco_t_clas_c_methods
        e_t_methods_private       TYPE sxco_t_clas_c_methods.

    METHODS _type_to_string
      IMPORTING
                i_o_type             TYPE REF TO if_xco_ao_c_type
      RETURNING VALUE(r_type_string) TYPE string.

    METHODS _constant_to_string
      IMPORTING
                i_o_constant             TYPE REF TO if_xco_ao_c_constant
      RETURNING VALUE(r_constant_string) TYPE string.

    METHODS _class_data_to_string
      IMPORTING
                i_o_class_data            TYPE REF TO if_xco_ao_c_class_data
      RETURNING VALUE(r_attribute_string) TYPE string.

    METHODS _data_to_string
      IMPORTING
                i_o_data                  TYPE REF TO if_xco_ao_c_data
      RETURNING VALUE(r_attribute_string) TYPE string.

    METHODS _importing_params_to_string
      IMPORTING
                i_t_importing_parameters   TYPE sxco_t_ao_s_p_importings
      RETURNING VALUE(r_parameters_string) TYPE string.

    METHODS _exporting_params_to_string
      IMPORTING
                i_t_exporting_parameters   TYPE sxco_t_ao_s_p_exportings
      RETURNING VALUE(r_parameters_string) TYPE string.

    METHODS _changing_params_to_string
      IMPORTING
                i_t_changing_parameters    TYPE sxco_t_ao_s_p_changings
      RETURNING VALUE(r_parameters_string) TYPE string.

    METHODS _returning_param_to_string
      IMPORTING
                i_t_returning_parameter    TYPE sxco_t_ao_s_p_returnings
      RETURNING VALUE(r_parameters_string) TYPE string.

    METHODS _exceptions_to_string
      IMPORTING
                i_t_exceptions             TYPE sxco_t_ao_s_exceptions
      RETURNING VALUE(r_exceptions_string) TYPE string.

ENDCLASS.



CLASS ycl_aaic_class_tools IMPLEMENTATION.


  METHOD create.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_package) = CONV sxco_package( condense( to_upper( i_package ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_put_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->create_put_operation( ).

    DATA(lo_specification) = lo_put_operation->for-clas->add_object( l_class_name
      )->set_package( l_package
      )->create_form_specification( ).

    lo_specification->set_short_description( i_description ).

    lo_specification->definition->set_create_visibility( xco_cp_abap_objects=>visibility->public ).

    IF i_interface_name IS SUPPLIED.
      DATA(l_interface_name) = CONV sxco_ad_object_name( condense( to_upper( i_interface_name ) ) ).
      lo_specification->definition->add_interface( l_interface_name ).
    ENDIF.

    IF i_superclass_name IS SUPPLIED.
      DATA(l_superclass_name) = CONV sxco_ad_object_name( condense( to_upper( i_superclass_name ) ) ).
      lo_specification->definition->set_superclass( l_superclass_name ).
    ENDIF.

    TRY.

        lo_put_operation->execute( ).

        r_response = |Class `{ l_class_name }` created successfully!|.

      CATCH cx_xco_gen_put_exception INTO DATA(lo_cx_xco_gen_put_exception).

        r_response = |Error: the class `{ l_class_name }` was not created.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_put_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.


  METHOD add_method.

    DATA lt_source TYPE STANDARD TABLE OF string.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(l_section) = CONV sxco_clas_method_name( to_upper( condense( i_class_section ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF i_static = abap_false.

        DATA(lo_method_definition) = lo_patch_operation_object->for-insert->definition->section-private->add_method( l_method_name
          )->set_short_description( i_description ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-insert->definition->section-private->add_class_method( CONV #( l_method_name )
          )->set_short_description( i_description ).

      ENDIF.

    ELSE.

      IF i_static = abap_false.

        lo_method_definition = lo_patch_operation_object->for-insert->definition->section-public->add_method( l_method_name
          )->set_short_description( i_description ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-insert->definition->section-public->add_class_method( CONV #( l_method_name )
          )->set_short_description( i_description ).

      ENDIF.

    ENDIF.

    LOOP AT i_t_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_params>).

      lo_method_definition->add_importing_parameter( CONV #( <ls_importing_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDLOOP.

    LOOP AT i_t_exporting_params ASSIGNING FIELD-SYMBOL(<ls_exporting_params>).

      lo_method_definition->add_exporting_parameter( CONV #( <ls_exporting_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_exporting_params>-type ) ).

    ENDLOOP.

    LOOP AT i_t_changing_params ASSIGNING FIELD-SYMBOL(<ls_changing_params>).

      lo_method_definition->add_changing_parameter( CONV #( <ls_changing_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_changing_params>-type ) ).

    ENDLOOP.

    IF i_s_returning_param IS NOT INITIAL.

      lo_method_definition->add_returning_parameter( CONV #( i_s_returning_param-name )
        )->set_type( xco_cp_abap=>type-source->for( i_s_returning_param-type ) ).

    ENDIF.

    LOOP AT i_t_exceptions ASSIGNING FIELD-SYMBOL(<ls_exception>).

      lo_method_definition->add_exception( CONV #( <ls_exception>-name ) ).

    ENDLOOP.

    SPLIT i_source AT cl_abap_char_utilities=>newline INTO TABLE lt_source.

    DATA(lo_method_implementation) = lo_patch_operation_object->for-insert->implementation->add_method( l_method_name
      )->set_source( lt_source ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Method `{ l_method_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Method `{ l_method_name }` was not added to class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD change_method_implementation.

    DATA lt_source TYPE STANDARD TABLE OF string.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    SPLIT i_source AT cl_abap_char_utilities=>newline INTO TABLE lt_source.

    DATA(lo_method_implementation) = lo_patch_operation_object->for-insert->implementation->add_method( l_method_name
      )->set_source( lt_source ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Source code of method `{ l_method_name }` in class `{ l_class_name }` was changed successfully.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Source code of method `{ l_method_name }` in class `{ l_class_name }` was not changed.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.


  METHOD add_method_parameters.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_method_section(
      EXPORTING
        i_class_name  = l_class_name
        i_method_name = CONV #( l_method_name )
      IMPORTING
        e_section     = DATA(l_section)
        e_static      = DATA(l_static)
    ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF l_static = abap_false.

        DATA(lo_method_definition) = lo_patch_operation_object->for-update->definition->section-private->add_method( l_method_name ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-private->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ELSE.

      IF l_static = abap_false.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-public->add_method( l_method_name ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-public->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ENDIF.

    LOOP AT i_t_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_params>).

      lo_method_definition->for-insert->add_importing_parameter( CONV #( <ls_importing_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_importing_params>-type ) ).

    ENDLOOP.

    LOOP AT i_t_exporting_params ASSIGNING FIELD-SYMBOL(<ls_exporting_params>).

      lo_method_definition->for-insert->add_exporting_parameter( CONV #( <ls_exporting_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_exporting_params>-type ) ).

    ENDLOOP.

    LOOP AT i_t_changing_params ASSIGNING FIELD-SYMBOL(<ls_changing_params>).

      lo_method_definition->for-insert->add_changing_parameter( CONV #( <ls_changing_params>-name )
        )->set_pass_by_reference(
        )->set_type( xco_cp_abap=>type-source->for( <ls_changing_params>-type ) ).

    ENDLOOP.

    IF i_s_returning_param IS NOT INITIAL.

      lo_method_definition->for-insert->add_returning_parameter( CONV #( i_s_returning_param-name )
        )->set_type( xco_cp_abap=>type-source->for( i_s_returning_param-type ) ).

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Parameter(s) added to method `{ l_method_name }` of class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Parameter(s) not added to method `{ l_method_name }` of class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD delete_method_parameters.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_method_section(
      EXPORTING
        i_class_name  = l_class_name
        i_method_name = CONV #( l_method_name )
      IMPORTING
        e_section     = DATA(l_section)
        e_static      = DATA(l_static)
    ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF l_static = abap_false.

        DATA(lo_method_definition) = lo_patch_operation_object->for-update->definition->section-private->add_method( l_method_name ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-private->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ELSE.

      IF l_static = abap_false.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-public->add_method( l_method_name ).

      ELSE.

        lo_method_definition = lo_patch_operation_object->for-update->definition->section-public->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ENDIF.

    LOOP AT i_t_importing_params ASSIGNING FIELD-SYMBOL(<ls_importing_params>).

      lo_method_definition->for-delete->add_importing_parameter( CONV #( <ls_importing_params>-name ) ).

    ENDLOOP.

    LOOP AT i_t_exporting_params ASSIGNING FIELD-SYMBOL(<ls_exporting_params>).

      lo_method_definition->for-delete->add_exporting_parameter( CONV #( <ls_exporting_params>-name ) ).

    ENDLOOP.

    LOOP AT i_t_changing_params ASSIGNING FIELD-SYMBOL(<ls_changingg_params>).

      lo_method_definition->for-delete->add_changing_parameter( CONV #( <ls_importing_params>-name ) ).

    ENDLOOP.

    IF i_s_returning_param IS NOT INITIAL.

      lo_method_definition->for-delete->add_returning_parameter( CONV #( i_s_returning_param-name ) ).

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Parameter(s) delete from method `{ l_method_name }` of class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Parameter(s) not deleted from method `{ l_method_name }` of class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD delete_method.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_method_section(
      EXPORTING
        i_class_name  = l_class_name
        i_method_name = CONV #( l_method_name )
      IMPORTING
        e_section     = DATA(l_section)
        e_static      = DATA(l_static)
    ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF l_static = abap_false.

        lo_patch_operation_object->for-delete->definition->section-private->add_method( l_method_name ).

      ELSE.

        lo_patch_operation_object->for-delete->definition->section-private->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ELSE.

      IF l_static = abap_false.

        lo_patch_operation_object->for-delete->definition->section-public->add_method( l_method_name ).

      ELSE.

        lo_patch_operation_object->for-delete->definition->section-public->add_class_method( CONV #( l_method_name ) ).

      ENDIF.

    ENDIF.

    TRY.

        DATA(lo_result) = lo_patch_operation->execute( ).

        IF lo_result->findings->contain_errors( ).
          r_response = |Method not deleted|.
        ELSE.
          r_response = |Method was deleted|.
        ENDIF.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Method `{ l_method_name }` was not deleted from class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.


  METHOD add_attribute.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_attribute_name) = CONV sxco_ao_component_name( condense( i_attribute_name ) ).

    DATA(l_section) = CONV sxco_clas_method_name( to_upper( condense( i_class_section ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF i_static = abap_false.

        DATA(lo_data) = lo_patch_operation_object->for-insert->definition->section-private->add_data( l_attribute_name
          )->set_type( xco_cp_abap=>type-source->for( i_attribute_type ) ).

        IF i_default_value IS NOT INITIAL.
          lo_data->set_string_value( i_default_value ).
        ENDIF.

      ELSE.

        DATA(lo_class_data) = lo_patch_operation_object->for-insert->definition->section-private->add_class_data( l_attribute_name
          )->set_type( xco_cp_abap=>type-source->for( i_attribute_type ) ).

        IF i_default_value IS NOT INITIAL.
          lo_class_data->set_string_value( i_default_value ).
        ENDIF.

      ENDIF.

    ELSE.

      IF i_static = abap_false.

        lo_data = lo_patch_operation_object->for-insert->definition->section-public->add_data( l_attribute_name
          )->set_type( xco_cp_abap=>type-source->for( i_attribute_type )
          )->set_read_only( i_read_only ).

        IF i_default_value IS NOT INITIAL.
          lo_data->set_string_value( i_default_value ).
        ENDIF.

      ELSE.

        lo_class_data = lo_patch_operation_object->for-insert->definition->section-public->add_class_data( l_attribute_name
          )->set_type( xco_cp_abap=>type-source->for( i_attribute_type )
          )->set_read_only( i_read_only ).

        IF i_default_value IS NOT INITIAL.
          lo_class_data->set_string_value( i_default_value ).
        ENDIF.

      ENDIF.

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Attribute `{ l_attribute_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Attribute `{ l_attribute_name }` was not added to class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD add_constant.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_constant_name) = CONV sxco_ao_component_name( condense( i_constant_name ) ).

    DATA(l_section) = CONV sxco_clas_method_name( to_upper( condense( i_class_section ) ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.


    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    DATA(lo_data) = lo_patch_operation_object->for-insert->definition->section-public->add_constant( l_constant_name
      )->set_type( xco_cp_abap=>type-source->for( CONV #( i_constant_type ) )
      )->set_string_value( i_constant_value ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Constant `{ l_constant_name }` added to class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Constant `{ l_constant_name }` was not added to class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD delete_attribute.

    CLEAR r_response.

    DATA lt_source TYPE STANDARD TABLE OF string.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_attribute_name) = CONV sxco_ao_component_name( condense( i_attribute_name ) ).

    IF xco_cp_abap=>class( l_class_name )->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_attribute_section(
      EXPORTING
        i_class_name     = l_class_name
        i_attribute_name = l_attribute_name
      IMPORTING
        e_section        = DATA(l_section)
        e_static         = DATA(l_static)
    ).

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).

    IF l_section = mc_private.

      IF l_static = abap_false.

        lo_patch_operation_object->for-delete->definition->section-private->add_data( l_attribute_name ).

      ELSE.

        lo_patch_operation_object->for-delete->definition->section-private->add_class_data( l_attribute_name ).

      ENDIF.

    ELSE.

      IF l_static = abap_false.

        lo_patch_operation_object->for-delete->definition->section-public->add_data( l_attribute_name ).

      ELSE.

        lo_patch_operation_object->for-delete->definition->section-public->add_class_data( l_attribute_name ).

      ENDIF.

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Attribute `{ l_attribute_name }` deleted from class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Attribute `{ l_attribute_name }` was not deleted from class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD delete_constant.

    CLEAR r_response.

    DATA lt_source TYPE STANDARD TABLE OF string.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_constant_name) = CONV sxco_ao_component_name( condense( i_constant_name ) ).

    DATA(lo_class) = xco_cp_abap=>class( l_class_name ).

    IF lo_class->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request
      )->for-clas->create_patch_operation( ).

    DATA(lo_patch_operation_object) = lo_patch_operation->add_object( l_class_name ).



    IF lo_class->definition->section-public->component->constant( l_constant_name )->exists( ) = abap_true.

      lo_patch_operation_object->for-delete->definition->section-public->add_constant( l_constant_name ).

    ELSE.

      lo_patch_operation_object->for-delete->definition->section-private->add_constant( l_constant_name ).

    ENDIF.

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Constant `{ l_constant_name }` deleted from class `{ l_class_name }`.|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error! Constant `{ l_constant_name }` was not deleted from class `{ l_class_name }`.|.

        me->_add_findings_to_response(
          EXPORTING
            i_o_findings = lo_cx_xco_gen_patch_exception->findings->for->clas
          CHANGING
            ch_response  = r_response
        ).

    ENDTRY.

  ENDMETHOD.

  METHOD get_class_definition.

    DATA: l_interfaces            TYPE string,
          l_public_types          TYPE string,
          l_private_types         TYPE string,
          l_public_constants      TYPE string,
          l_private_constants     TYPE string,
          l_public_class_data     TYPE string,
          l_private_class_data    TYPE string,
          l_public_class_methods  TYPE string,
          l_private_class_methods TYPE string,
          l_public_data           TYPE string,
          l_private_data          TYPE string,
          l_public_methods        TYPE string,
          l_private_methods       TYPE string.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    TRY.

        DATA(lo_class) = xco_cp_abap=>class( l_class_name ).

        IF lo_class->exists( ) = abap_false.
          r_response = |The class `{ l_class_name }` does not exist.|.
          RETURN.
        ENDIF.

        DATA(lt_interfaces) = lo_class->definition->content( )->get_interfaces( ).

        DATA(lo_superclass) = lo_class->definition->content( )->get_superclass( ).

        DATA(lt_public_types) = lo_class->definition->section-public->components->type->all->get( ).

        DATA(lt_private_types) = lo_class->definition->section-private->components->type->all->get( ).

        DATA(lt_public_constants) = lo_class->definition->section-public->components->constant->all->get( ).

        DATA(lt_private_constants) = lo_class->definition->section-private->components->constant->all->get( ).

        me->_get_attributes(
          EXPORTING
            io_class                = lo_class
          IMPORTING
            e_t_class_datas_public  = DATA(lt_public_class_data)
            e_t_datas_public        = DATA(lt_public_data)
            e_t_class_datas_private = DATA(lt_private_class_data)
            e_t_datas_private       = DATA(lt_private_data)
        ).

        me->_get_methods(
          EXPORTING
            io_class                  = lo_class
          IMPORTING
            e_t_class_methods_public  = DATA(lt_public_class_methods)
            e_t_methods_public        = DATA(lt_public_methods)
            e_t_class_methods_private = DATA(lt_private_class_methods)
            e_t_methods_private       = DATA(lt_private_methods)
        ).

        LOOP AT lt_interfaces ASSIGNING FIELD-SYMBOL(<lo_interface>).
          IF l_interfaces IS NOT INITIAL.
            l_interfaces = |{ l_interfaces }{ cl_abap_char_utilities=>newline }|.
          ENDIF.
          l_interfaces = |{ l_interfaces }INTERFACES { <lo_interface>->name }.|.
        ENDLOOP.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.

        r_response = |Error while reading the definition of the class `{ l_class_name }`.|.

        RETURN.

    ENDTRY.

    " CLASS DEFINITION
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    r_response = |CLASS { l_class_name } DEFINITION.{ cl_abap_char_utilities=>newline }{ cl_abap_char_utilities=>newline }|.

    " PUBLIC SECTION
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    r_response = |{ r_response }PUBLIC SECTION.{ cl_abap_char_utilities=>newline }|.

    IF l_interfaces IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_interfaces }|.
    ENDIF.

    " PUBLIC TYPES
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_public_types ASSIGNING FIELD-SYMBOL(<ls_public_type>).

      DATA(l_type) = me->_type_to_string( <ls_public_type> ).

      IF l_type IS NOT INITIAL.
        l_public_types = |{ l_public_types }{ cl_abap_char_utilities=>newline }|.
        l_public_types = |{ l_public_types }TYPES: { l_type }.|.
      ENDIF.

    ENDLOOP.

    IF l_public_types IS NOT INITIAL.
*      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_public_types }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PUBLIC CONSTANTS
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_public_constants ASSIGNING FIELD-SYMBOL(<ls_public_constant>).

      DATA(l_constant) = me->_constant_to_string( <ls_public_constant> ).

      IF l_constant IS NOT INITIAL.
        l_public_constants = |{ l_public_constants }{ cl_abap_char_utilities=>newline }|.
        l_public_constants = |{ l_public_constants }CONSTANTS { l_constant }.|.
      ENDIF.

    ENDLOOP.

    IF l_public_constants IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_public_constants }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PUBLIC CLASS-DATA
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_public_class_data ASSIGNING FIELD-SYMBOL(<ls_public_class_data>).

      DATA(l_class_data) = me->_class_data_to_string( <ls_public_class_data> ).

      IF l_class_data IS NOT INITIAL.
        l_public_class_data = |{ l_public_class_data }{ cl_abap_char_utilities=>newline }|.
        l_public_class_data = |{ l_public_class_data }CLASS-DATA { l_class_data }.|.
      ENDIF.

    ENDLOOP.

    IF l_public_class_data IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_public_class_data }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PUBLIC DATA
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_public_data ASSIGNING FIELD-SYMBOL(<ls_public_data>).

      DATA(l_data) = me->_data_to_string( <ls_public_data> ).

      IF l_data IS NOT INITIAL.
        l_public_data = |{ l_public_data }{ cl_abap_char_utilities=>newline }|.
        l_public_data = |{ l_public_data }DATA { l_data }.|.
      ENDIF.

    ENDLOOP.

    IF l_public_data IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_public_data }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PUBLIC CLASS-METHODS
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_public_class_methods ASSIGNING FIELD-SYMBOL(<ls_public_class_method>).

      DATA(l_method_definition) = me->get_method_definition(
        EXPORTING
          i_class_name  = l_class_name
          i_method_name = CONV #( <ls_public_class_method>->name )
      ).

      l_public_class_methods = |{ l_public_class_methods }{ cl_abap_char_utilities=>newline }|.

      l_public_class_methods = |{ l_public_class_methods }{ l_method_definition }|.

    ENDLOOP.

    IF l_public_class_methods IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_public_class_methods }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PUBLIC METHODS
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_public_methods ASSIGNING FIELD-SYMBOL(<ls_public_method>).

      l_method_definition = me->get_method_definition(
        EXPORTING
          i_class_name  = l_class_name
          i_method_name = CONV #( <ls_public_method>->name )
      ).

      l_public_methods = |{ l_public_methods }{ cl_abap_char_utilities=>newline }|.

      l_public_methods = |{ l_public_methods }{ l_method_definition }|.

    ENDLOOP.

    IF l_public_methods IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_public_methods }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PRIVATE SECTION
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
    r_response = |{ r_response }PRIVATE SECTION.{ cl_abap_char_utilities=>newline }|.


    " PRIVATE TYPES
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_private_types ASSIGNING FIELD-SYMBOL(<ls_private_type>).

      l_type = me->_type_to_string( <ls_private_type> ).

      IF l_type IS NOT INITIAL.
        l_private_types = |{ l_private_types }{ cl_abap_char_utilities=>newline }|.
        l_private_types = |{ l_private_types }TYPES: { l_type }.|.
      ENDIF.

    ENDLOOP.

    IF l_private_types IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_private_types }{ cl_abap_char_utilities=>newline }|.
    ENDIF.


    " PRIVATE CONSTANTS
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_private_constants ASSIGNING FIELD-SYMBOL(<ls_private_constant>).

      l_constant = me->_constant_to_string( <ls_private_constant> ).

      IF l_constant IS NOT INITIAL.
        l_private_constants = |{ l_private_constants }{ cl_abap_char_utilities=>newline }|.
        l_private_constants = |{ l_private_constants }CONSTANTS { l_constant }.|.
      ENDIF.

    ENDLOOP.

    IF l_private_constants IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_private_constants }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PRIVATE CLASS-DATA
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_private_class_data ASSIGNING FIELD-SYMBOL(<ls_private_class_data>).

      l_class_data = me->_class_data_to_string( <ls_private_class_data> ).

      IF l_class_data IS NOT INITIAL.
        l_private_class_data = |{ l_private_class_data }{ cl_abap_char_utilities=>newline }|.
        l_private_class_data = |{ l_private_class_data }CLASS-DATA { l_class_data }.|.
      ENDIF.

    ENDLOOP.

    IF l_private_class_data IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_private_class_data }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PRIVATE DATA
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_private_data ASSIGNING FIELD-SYMBOL(<ls_private_data>).

      l_data = me->_data_to_string( <ls_private_data> ).

      IF l_data IS NOT INITIAL.
        l_private_data = |{ l_private_data }{ cl_abap_char_utilities=>newline }|.
        l_private_data = |{ l_private_data }DATA { l_data }.|.
      ENDIF.

    ENDLOOP.

    IF l_private_data IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_private_data }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PRIVATE CLASS-METHODS
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_private_class_methods ASSIGNING FIELD-SYMBOL(<ls_private_class_method>).

      l_method_definition = me->get_method_definition(
        EXPORTING
          i_class_name  = l_class_name
          i_method_name = CONV #( <ls_private_class_method>->name )
      ).

      l_private_class_methods = |{ l_private_class_methods }{ cl_abap_char_utilities=>newline }|.

      l_private_class_methods = |{ l_private_class_methods }{ l_method_definition }|.

    ENDLOOP.

    IF l_private_class_methods IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_private_class_methods }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

    " PRIVATE METHODS
    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    LOOP AT lt_private_methods ASSIGNING FIELD-SYMBOL(<ls_private_method>).

      l_method_definition = me->get_method_definition(
        EXPORTING
          i_class_name  = l_class_name
          i_method_name = CONV #( <ls_private_method>->name )
      ).

      l_private_methods = |{ l_private_methods }{ cl_abap_char_utilities=>newline }|.

      l_private_methods = |{ l_private_methods }{ l_method_definition }|.

    ENDLOOP.

    IF l_private_methods IS NOT INITIAL.
      r_response = |{ r_response }{ cl_abap_char_utilities=>newline }|.
      r_response = |{ r_response }{ l_private_methods }{ cl_abap_char_utilities=>newline }|.
    ENDIF.

  ENDMETHOD.

  METHOD get_method_definition.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(lo_class) = xco_cp_abap=>class( l_class_name ).

    IF lo_class->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_method_section(
      EXPORTING
        i_class_name  = l_class_name
        i_method_name = CONV #( l_method_name )
      IMPORTING
        e_section     = DATA(l_section)
        e_static      = DATA(l_static)
    ).

    IF l_section IS INITIAL.
      r_response = |The method `{ l_method_name }` does not exist in class `{ l_class_name }`.|.
      RETURN.
    ENDIF.

    TRY.

        IF l_section = mc_public.

          IF l_static = abap_true.

            DATA(lo_method) = lo_class->definition->section-public->component->class_method( l_method_name ).

          ELSE.

            lo_method = lo_class->definition->section-public->component->method( l_method_name ).

          ENDIF.

        ELSE.

          IF l_static = abap_true.

            lo_method = lo_class->definition->section-private->component->class_method( l_method_name ).

          ELSE.

            lo_method = lo_class->definition->section-private->component->method( l_method_name ).

          ENDIF.

        ENDIF.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.

        r_response = |Error while reading the definition of the method `{ l_method_name }` of class `{ l_class_name }`.|.

        RETURN.

    ENDTRY.

    IF lo_method IS NOT BOUND.
      r_response = |Error while reading the definition of the method `{ l_method_name }` of class `{ l_class_name }`.|.
      RETURN.
    ENDIF.

    IF l_static = abap_true.
      r_response = |CLASS-METHODS { l_method_name }|.
    ELSE.
      r_response = |METHODS { l_method_name }|.
    ENDIF.

    " IMPORTING parameters
    r_response = |{ r_response }{ me->_importing_params_to_string( lo_method->importing_parameters->all->get( ) ) }|.

    " EXPORTING parameters
    r_response = |{ r_response }{ me->_exporting_params_to_string( lo_method->exporting_parameters->all->get( ) ) }|.

    " CHANGING parameters
    r_response = |{ r_response }{ me->_changing_params_to_string( lo_method->changing_parameters->all->get( ) ) }|.

    " RETURNING parameters
    r_response = |{ r_response }{ me->_returning_param_to_string( lo_method->returning_parameters->all->get( ) ) }|.

    " EXCEPTIONS
    r_response = |{ r_response }{ me->_exceptions_to_string( lo_method->exceptions->all->get( ) ) }|.

    r_response = |{ r_response }.|.

  ENDMETHOD.

  METHOD get_method_implementation.

    CLEAR r_response.

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(l_method_name) = CONV sxco_clas_method_name( condense( to_upper( i_method_name ) ) ).

    DATA(lo_class) = xco_cp_abap=>class( l_class_name ).

    IF lo_class->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    me->_get_method_section(
      EXPORTING
        i_class_name  = l_class_name
        i_method_name = CONV #( l_method_name )
      IMPORTING
        e_section     = DATA(l_section)
        e_static      = DATA(l_static)
    ).

    IF l_section IS INITIAL.
      r_response = |The method `{ l_method_name }` does not exist in class `{ l_class_name }`.|.
      RETURN.
    ENDIF.

    DATA(ls_implementation) = lo_class->implementation->method( l_method_name )->content( )->get( ).

    r_response = |  METHOD { to_lower( l_method_name ) }.{ cl_abap_char_utilities=>cr_lf }|.

    r_response = |{ r_response }{ xco_cp=>strings( ls_implementation-source )->join( |{ cl_abap_char_utilities=>cr_lf }| )->value }|.

    r_response = |{ r_response }{ cl_abap_char_utilities=>cr_lf }  ENDMETHOD.|.

  ENDMETHOD.

  METHOD get_translation.

    CLEAR r_response.

    DATA(l_language) = i_language.

    l_language = to_upper( l_language ).

    DATA(lo_language) = xco_cp=>language( l_language ).

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(lo_class) = xco_cp_abap=>class( l_class_name ).

    IF lo_class->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(lo_text_attribute) = xco_cp_text_pool=>text_attribute->text_element_text.

    TRY.

        DATA(lo_target) = xco_cp_i18n=>target->text_pool->class_text_symbol( iv_class_name = l_class_name
                                                                             iv_text_symbol_id = CONV #( i_text_symbol_id ) ).

        DATA(lo_translation) = lo_target->get_translation( io_language = lo_language
                                                           it_text_attributes = VALUE #( ( lo_text_attribute ) ) ).

        LOOP AT lo_translation->texts INTO DATA(lo_text).
          r_response = lo_text_attribute->if_xco_i18n_text_attribute~get_string_for_text( lo_text->value ).
        ENDLOOP.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.

        r_response = |The text symbol id `{ i_text_symbol_id }` does not exist in the class `{ l_class_name }`.|.

    ENDTRY.

  ENDMETHOD.

  METHOD set_translation.

    CLEAR r_response.

    DATA(l_language) = i_language.

    l_language = to_upper( l_language ).

    DATA(lo_language) = xco_cp=>language( l_language ).

    DATA(l_class_name) = CONV sxco_ad_object_name( condense( to_upper( i_class_name ) ) ).

    DATA(lo_class) = xco_cp_abap=>class( l_class_name ).

    IF lo_class->exists( ) = abap_false.
      r_response = |The class `{ l_class_name }` does not exist.|.
      RETURN.
    ENDIF.

    DATA(l_transport_request) = me->_get_transport_request( l_class_name ).

    IF l_transport_request IS INITIAL.
      l_transport_request = condense( to_upper( i_transport_request ) ).
    ENDIF.

    DATA(lo_transport_request) = xco_cp_cts=>transport->for( iv_transport = l_transport_request ).

    DATA(lo_text_attribute) = xco_cp_text_pool=>text_attribute->text_element_text.

    DATA(lo_text) = lo_text_attribute->create_text( xco_cp=>string( i_text ) ).

    TRY.

        DATA(lo_target) = xco_cp_i18n=>target->text_pool->class_text_symbol( iv_class_name = l_class_name
                                                                             iv_text_symbol_id = CONV #( i_text_symbol_id ) ).

        lo_target->set_translation(
          it_texts           = VALUE #( ( lo_text ) )
          io_language        = lo_language
          io_change_scenario = lo_transport_request
        ).

        r_response = |The translation of text symbol id `{ i_text_symbol_id }` of class `{ l_class_name }` was saved.|.

      CATCH cx_xco_runtime_exception ##NO_HANDLER.

        r_response = |Error! The translation of text symbol id `{ i_text_symbol_id }` of class `{ l_class_name }` was not saved.|.

    ENDTRY.

  ENDMETHOD.

  METHOD _add_findings_to_response.

    DATA(lt_findings) = i_o_findings->get( ).

    LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

      IF ch_response IS NOT INITIAL.
        ch_response = ch_response && cl_abap_char_utilities=>newline.
      ENDIF.

      LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

        ch_response = ch_response && <lo_message>->get_text( ).

      ENDLOOP.

    ENDLOOP.

  ENDMETHOD.


  METHOD _get_attribute_section.

    CLEAR: e_section, e_static.

    DATA(lo_class) = xco_cp_abap=>class( i_class_name ).

    IF lo_class->definition->section-public->component->data( i_attribute_name )->exists( ).
      e_section = mc_public.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-private->component->data( i_attribute_name )->exists( ).
      e_section = mc_private.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-public->component->class_data( i_attribute_name )->exists( ).
      e_section = mc_public.
      e_static = abap_true.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-private->component->class_data( i_attribute_name )->exists( ).
      e_section = mc_private.
      e_static = abap_true.
    ENDIF.

  ENDMETHOD.


  METHOD _get_method_section.

    CLEAR: e_section, e_static.

    DATA(lo_class) = xco_cp_abap=>class( i_class_name ).

    IF lo_class->definition->section-public->component->method( CONV #( i_method_name ) )->exists( ).
      e_section = mc_public.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-private->component->method( CONV #( i_method_name ) )->exists( ).
      e_section = mc_private.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-public->component->class_method( CONV #( i_method_name ) )->exists( ).
      e_section = mc_public.
      e_static = abap_true.
      RETURN.
    ENDIF.

    IF lo_class->definition->section-private->component->class_method( CONV #( i_method_name ) )->exists( ).
      e_section = mc_private.
      e_static = abap_true.
    ENDIF.

  ENDMETHOD.

  METHOD _get_package.

    CLEAR r_package.

    DATA(lo_class) = xco_cp_abap=>class( i_class_name ).

    IF lo_class->exists( ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lo_package) = lo_class->if_xco_ar_object~get_package( ).

    r_package = lo_package->name.

  ENDMETHOD.

  METHOD _get_transport_request.

    CLEAR r_transport_request.

    DATA(lo_class) = xco_cp_abap=>class( i_class_name ).

    IF lo_class->exists( ) = abap_false.
      RETURN.
    ENDIF.

    DATA(lo_lock) = lo_class->if_xco_cts_changeable~get_object( )->get_lock( ).

    IF lo_lock->exists( ) = abap_true.
      r_transport_request = lo_lock->get_transport( ).
    ENDIF.

  ENDMETHOD.

  METHOD _get_attributes.

    e_t_class_datas_public = io_class->definition->section-public->components->class_data->all->get( ).

    e_t_datas_public = io_class->definition->section-public->components->data->all->get( ).

    e_t_class_datas_private = io_class->definition->section-private->components->class_data->all->get( ).

    e_t_datas_private = io_class->definition->section-private->components->data->all->get( ).

  ENDMETHOD.

  METHOD _get_methods.

    e_t_class_methods_public = io_class->definition->section-public->components->class_method->all->get( ).

    e_t_methods_public = io_class->definition->section-public->components->method->all->get( ).

    e_t_class_methods_private = io_class->definition->section-private->components->class_method->all->get( ).

    e_t_methods_private = io_class->definition->section-private->components->method->all->get( ).

  ENDMETHOD.

  METHOD _type_to_string.

    CLEAR r_type_string.

    DATA(lo_content) = i_o_type->content( ).

    DATA(l_name) = lo_content->type->name.

    DATA(ls_type) = lo_content->get( ).

    DATA(l_type) = ls_type-typing_definition->get_value( ).

    DATA(l_source) = ls_type-typing_definition->get_source( ).

    IF l_source IS NOT INITIAL.
      r_type_string = l_source.
      RETURN.
    ENDIF.

    r_type_string = |{ l_name } TYPE { l_type }|.

  ENDMETHOD.

  METHOD _constant_to_string.

    CLEAR r_constant_string.

    DATA(lo_content) = i_o_constant->content( ).

    DATA(l_name) = lo_content->constant->name.

    DATA(ls_constant) = lo_content->get( ).

    DATA(l_type) = ls_constant-typing_definition->get_value( ).

    DATA(l_source) = ls_constant-typing_definition->get_source( ).

    DATA(l_type_ref) = ls_constant-typing_method->if_xco_printable~get_text( )->get_lines( )->join( )->value.

    DATA(l_value) = ls_constant-value.

    IF l_type IS INITIAL.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN l_source WITH space.
      r_constant_string = |{ l_source } VALUE { l_value }|.
      RETURN.
    ENDIF.

    r_constant_string = |{ l_name } TYPE { l_type } VALUE { l_value }|.

  ENDMETHOD.

  METHOD _class_data_to_string.

    CLEAR r_attribute_string.

    DATA(lo_content) = i_o_class_data->content( ).

    DATA(l_name) = lo_content->class_data->name.

    DATA(ls_class_data) = lo_content->get( ).

    DATA(l_type) = ls_class_data-typing_definition->get_value( ).
    DATA(l_source) = ls_class_data-typing_definition->get_source( ).

    DATA(l_type_ref) = ls_class_data-typing_method->if_xco_printable~get_text( )->get_lines( )->join( )->value.

    IF l_type IS INITIAL.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN l_source WITH space.
      r_attribute_string = l_source.
      RETURN.
    ENDIF.

    IF l_type_ref = 'OBJECT_REFERENCE'.
      r_attribute_string = |{ l_name } TYPE REF TO { l_type }|.
    ELSE.
      r_attribute_string = |{ l_name } TYPE { l_type }|.
    ENDIF.

    IF ls_class_data-read_only_indicator = abap_true.
      r_attribute_string = |{ r_attribute_string } READ-ONLY|.
    ENDIF.

  ENDMETHOD.

  METHOD _data_to_string.

    CLEAR r_attribute_string.

    DATA(lo_content) = i_o_data->content( ).

    DATA(l_name) = lo_content->data->name.

    DATA(ls_data) = lo_content->get( ).

    DATA(l_type) = ls_data-typing_definition->get_value( ).

    DATA(l_source) = ls_data-typing_definition->get_source( ).

    DATA(l_type_ref) = ls_data-typing_method->if_xco_printable~get_text( )->get_lines( )->join( )->value.

    IF l_type IS INITIAL.
      REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN l_source WITH space.
      r_attribute_string = l_source.
      RETURN.
    ENDIF.

    IF l_type_ref = 'OBJECT_REFERENCE'.
      r_attribute_string = |{ l_name } TYPE REF TO { l_type }|.
    ELSE.
      r_attribute_string = |{ l_name } TYPE { l_type }|.
    ENDIF.

    IF ls_data-read_only_indicator = abap_true.
      r_attribute_string = |{ r_attribute_string } READ-ONLY|.
    ENDIF.

  ENDMETHOD.

  METHOD _importing_params_to_string.

    CLEAR r_parameters_string.

    IF i_t_importing_parameters IS INITIAL.
      RETURN.
    ENDIF.

    r_parameters_string = |{ cl_abap_char_utilities=>newline }IMPORTING|.

    LOOP AT i_t_importing_parameters ASSIGNING FIELD-SYMBOL(<lo_importing_parameter>).

      r_parameters_string = |{ r_parameters_string }{ cl_abap_char_utilities=>newline }|.

      r_parameters_string = |{ r_parameters_string }{ <lo_importing_parameter>->name }|.

      DATA(ls_parameter) = <lo_importing_parameter>->content( )->get( ).

      DATA(l_type) = ls_parameter-typing_definition->get_value( ).

      " XCO Read API has bug reading the code generated by XCO generation API.
      " Without activating the class in Eclipse the XCO Read API doesn't read the properties of the importing parameter correctly.
      " The parameter type is read with all text that comes after TYPE (example: REF TO, DEFAULT, OPTIONAL)
      SPLIT l_type AT space INTO DATA(l_xco_bug_1) DATA(l_xco_bug_2).

      IF l_xco_bug_2 IS INITIAL.

        DATA(l_type_ref) = ls_parameter-typing_method->if_xco_printable~get_text( )->get_lines( )->join( )->value.

        IF l_type_ref = 'OBJECT_REFERENCE'.
          r_parameters_string = |{ r_parameters_string } TYPE REF TO { l_type }|.
        ELSE.
          r_parameters_string = |{ r_parameters_string } TYPE { l_type }|.
        ENDIF.

        IF ls_parameter-default_value IS NOT INITIAL.
          r_parameters_string = |{ r_parameters_string } DEFAULT { ls_parameter-default_value }|.
        ENDIF.

        IF ls_parameter-optional_indicator = abap_true.
          r_parameters_string = |{ r_parameters_string } OPTIONAL|.
        ENDIF.

      ELSE.

        " Workaround for the XCO Read API bug.
        " Variable l_type (typing_definition) has all text that comes after TYPE (example: REF TO, DEFAULT, OPTIONAL)
        r_parameters_string = |{ r_parameters_string } TYPE { l_type }|.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD _exporting_params_to_string.

    CLEAR r_parameters_string.

    IF i_t_exporting_parameters IS INITIAL.
      RETURN.
    ENDIF.

    r_parameters_string = |{ cl_abap_char_utilities=>newline }EXPORTING|.

    LOOP AT i_t_exporting_parameters ASSIGNING FIELD-SYMBOL(<lo_exporting_parameter>).

      r_parameters_string = |{ r_parameters_string }{ cl_abap_char_utilities=>newline }|.

      r_parameters_string = |{ r_parameters_string }{ <lo_exporting_parameter>->name }|.

      DATA(ls_parameter) = <lo_exporting_parameter>->content( )->get( ).

      DATA(l_type) = ls_parameter-typing_definition->get_value( ).

      DATA(l_type_ref) = ls_parameter-typing_method->if_xco_printable~get_text( )->get_lines( )->join( )->value.

      IF l_type_ref = 'OBJECT_REFERENCE'.
        r_parameters_string = |{ r_parameters_string } TYPE REF TO { l_type }|.
      ELSE.
        r_parameters_string = |{ r_parameters_string } TYPE { l_type }|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD _changing_params_to_string.

    CLEAR r_parameters_string.

    IF i_t_changing_parameters IS INITIAL.
      RETURN.
    ENDIF.

    r_parameters_string = |{ cl_abap_char_utilities=>newline }CHANGING|.

    LOOP AT i_t_changing_parameters ASSIGNING FIELD-SYMBOL(<lo_changing_parameter>).

      r_parameters_string = |{ r_parameters_string }{ cl_abap_char_utilities=>newline }|.

      r_parameters_string = |{ r_parameters_string }{ <lo_changing_parameter>->name }|.

      DATA(ls_parameter) = <lo_changing_parameter>->content( )->get( ).

      DATA(l_type) = ls_parameter-typing_definition->get_value( ).

      " XCO Read API has bug reading the code generated by XCO generation API.
      " Without activating the class in Eclipse the XCO Read API doesn't read the properties of the importing parameter correctly.
      " The parameter type is read with all text that comes after TYPE (example: REF TO, DEFAULT, OPTIONAL)
      SPLIT l_type AT space INTO DATA(l_xco_bug_1) DATA(l_xco_bug_2).

      IF l_xco_bug_2 IS INITIAL.

        DATA(l_type_ref) = ls_parameter-typing_method->if_xco_printable~get_text( )->get_lines( )->join( )->value.

        IF l_type_ref = 'OBJECT_REFERENCE'.
          r_parameters_string = |{ r_parameters_string } TYPE REF TO { l_type }|.
        ELSE.
          r_parameters_string = |{ r_parameters_string } TYPE { l_type }|.
        ENDIF.

        IF ls_parameter-default_value IS NOT INITIAL.
          r_parameters_string = |{ r_parameters_string } DEFAULT { ls_parameter-default_value }|.
        ENDIF.

        IF ls_parameter-optional_indicator = abap_true.
          r_parameters_string = |{ r_parameters_string } OPTIONAL|.
        ENDIF.

      ELSE.

        " Workaround for the XCO Read API bug.
        " Variable l_type (typing_definition) has all text that comes after TYPE (example: REF TO, DEFAULT, OPTIONAL)
        r_parameters_string = |{ r_parameters_string } TYPE { l_type }|.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD _returning_param_to_string.

    CLEAR r_parameters_string.

    IF i_t_returning_parameter IS INITIAL.
      RETURN.
    ENDIF.

    r_parameters_string = |{ cl_abap_char_utilities=>newline }RETURNING|.

    LOOP AT i_t_returning_parameter ASSIGNING FIELD-SYMBOL(<lo_returning_parameter>).

      r_parameters_string = |{ r_parameters_string }{ cl_abap_char_utilities=>newline }|.

      r_parameters_string = |{ r_parameters_string }VALUE({ <lo_returning_parameter>->name })|.

      DATA(ls_parameter) = <lo_returning_parameter>->content( )->get( ).

      DATA(l_type) = ls_parameter-typing_definition->get_value( ).

      DATA(l_type_ref) = ls_parameter-typing_method->if_xco_printable~get_text( )->get_lines( )->join( )->value.

      IF l_type_ref = 'OBJECT_REFERENCE'.
        r_parameters_string = |{ r_parameters_string } TYPE REF TO { l_type }|.
      ELSE.
        r_parameters_string = |{ r_parameters_string } TYPE { l_type }|.
      ENDIF.

    ENDLOOP.

  ENDMETHOD.

  METHOD _exceptions_to_string.

    CLEAR r_exceptions_string.

    IF i_t_exceptions IS INITIAL.
      RETURN.
    ENDIF.

    r_exceptions_string = |{ cl_abap_char_utilities=>newline }RAISING|.

    LOOP AT i_t_exceptions ASSIGNING FIELD-SYMBOL(<lo_exception>).

      r_exceptions_string = |{ r_exceptions_string }{ cl_abap_char_utilities=>newline }|.

      r_exceptions_string = |{ r_exceptions_string }{ <lo_exception>->name }|.

      DATA(ls_parameter) = <lo_exception>->content( )->get( ).

      r_exceptions_string = |{ r_exceptions_string } { ls_parameter-short_description }|.

    ENDLOOP.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_add_method) = abap_false.
    DATA(l_add_class_method) = abap_false.
    DATA(l_delete) = abap_false.
    DATA(l_add_attribute) = abap_false.
    DATA(l_add_constant) = abap_false.
    DATA(l_delete_attribute) = abap_false.
    DATA(l_delete_constant) = abap_true.
    DATA(l_change_method_implementation) = abap_false.
    DATA(l_add_method_parameters) = abap_false.
    DATA(l_delete_method_parameters) = abap_false.
    DATA(l_get_method_section) = abap_false.
    DATA(l_get_translation) = abap_false.
    DATA(l_set_translation) = abap_false.
    DATA(l_get_method_definition) = abap_false.
    DATA(l_get_method_implementation) = abap_false.
    DATA(l_get_class_definition) = abap_false.

    IF l_create = abap_true.

      l_response = me->create(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_interface_name    = 'YIF_AAIC_CHAT'
          i_description       = 'XCO ABAP Class generation'
          i_transport_request = 'TRLK900008'
          i_package           = 'ZCHRJS'
      ).

    ENDIF.

    IF l_add_method = abap_true.

      l_response = me->add_method(
        EXPORTING
          i_class_name         = 'ZCL_CJS_00001'
          i_method_name        = 'M1'
          i_description        = 'Method M1'
          i_transport_request  = 'TRLK900008'
          i_source             = |IF 1 = 2. { cl_abap_char_utilities=>newline } ENDIF.|
          i_t_importing_params = VALUE #( ( name = 'P1' type = |YDE_AAIC_API DEFAULT 'OPENAI'| )
                                          ( name = 'P2' type = 'STRING' )
                                          ( name = 'P3' type = 'REF TO YIF_AAIC_DB OPTIONAL' ) )
*          i_t_exporting_params = VALUE #( ( name = 'E_API' type = |YDE_AAIC_API| )
*                                          ( name = 'E_STR' type = 'STRING' )
*                                          ( name = 'E_CHAT' type = 'REF TO YIF_AAIC_CHAT' ) )
*          i_t_changing_params  = VALUE #( ( name = 'CH_API' type = |YDE_AAIC_API| )
*                                          ( name = 'CH_STR' type = 'STRING' )
*                                          ( name = 'CH_CHAT' type = 'REF TO YIF_AAIC_CHAT' ) )
          i_s_returning_param = VALUE #( name = 'r_api' type = |YDE_AAIC_API| )
          i_t_exceptions = VALUE #( ( name = 'CX_XCO_GEN_PATCH_EXCEPTION' ) )
        ).

    ENDIF.

    IF l_add_class_method = abap_true.

      l_response = me->add_method(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'CM1'
          i_static            = abap_true
          i_description       = 'Chat'
          i_transport_request = 'TRLK900008'
          i_source            = |IF 1 = 2.{ cl_abap_char_utilities=>newline }  "Class Method{ cl_abap_char_utilities=>newline }ENDIF.|
          i_t_importing_params = VALUE #( ( name = 'P1' type = |YDE_AAIC_API DEFAULT 'OPENAI'| )
                                          ( name = 'P2' type = 'STRING' )
                                          ( name = 'P3' type = 'REF TO YIF_AAIC_DB OPTIONAL' ) )
        ).

    ENDIF.

    IF l_delete = abap_true.

      l_response = me->delete_method(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'M2'
          i_transport_request = 'TRLK900008'
      ).

    ENDIF.

    IF l_add_attribute = abap_true.

      l_response = me->add_attribute(
                     i_class_name        = 'ZCL_CJS_00001'
                     i_attribute_name    = 'ATTR2'
                     i_attribute_type    = 'REF TO yif_aaic_chat'
                     i_transport_request = 'TRLK900008'
                   ).

    ENDIF.

    IF l_add_constant = abap_true.

      l_response = me->add_constant(
                     i_class_name       = 'ZCL_CJS_00001'
                     i_constant_name    = 'MC1'
                     i_constant_type    = 'string'
                     i_constant_value   = 'XPTO'
                     i_transport_request = 'TRLK900008'
                   ).

    ENDIF.

    IF l_delete_attribute = abap_true.

      l_response = me->delete_attribute(
                     i_class_name        = 'ZCL_CJS_00001'
                     i_attribute_name    = 'ATTR2'
                     i_transport_request = 'TRLK900008'
                   ).

    ENDIF.

    IF l_delete_constant = abap_true.

      l_response = me->delete_constant(
                     i_class_name        = 'ZCL_CJS_00001'
                     i_constant_name     = 'MC1'
                     i_transport_request = 'TRLK900008'
                   ).

    ENDIF.

    IF l_change_method_implementation = abap_true.

      l_response = me->change_method_implementation(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'YIF_AAIC_CHAT~CHAT'
*          i_transport_request = 'TRLK900008'
          i_source            = |IF 1 = 2. { cl_abap_char_utilities=>newline } ENDIF.|
      ).

    ENDIF.

    IF l_add_method_parameters = abap_true.

      l_response = me->add_method_parameters(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'CM1'
          i_transport_request = 'TRLK900008'
          i_t_importing_params = VALUE #( ( name = 'P4' type = |abap_boolean| ) )
      ).

    ENDIF.

    IF l_delete_method_parameters = abap_true.

      l_response = me->delete_method_parameters(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_method_name       = 'M2'
          i_transport_request = 'TRLK900008'
          i_t_importing_params = VALUE #( ( name = 'P3' ) )
      ).

    ENDIF.

    IF l_get_method_section = abap_true.

      out->write( '**get_method_section**' ).
      out->write( '------------------------------------------' ).
      out->write( 'i_class_name  = ZCL_CJS_00001' ).
      out->write( 'i_method_name = CM1' ).

      me->_get_method_section(
         EXPORTING
           i_class_name  = 'ZCL_CJS_00001'
           i_method_name = 'CM1'
         IMPORTING
           e_section = l_response
           e_static = DATA(l_static)
      ).

      IF l_static = abap_true.
        out->write( 'STATIC METHOD' ).
      ENDIF.

    ENDIF.

    IF l_get_translation = abap_true.

      l_response = me->get_translation(
        EXPORTING
          i_class_name     = 'ZCL_CJS_00001'
          i_text_symbol_id = '002'
          i_language       = 'P'
      ).

    ENDIF.

    IF l_set_translation = abap_true.

      l_response = me->set_translation(
        EXPORTING
          i_class_name        = 'ZCL_CJS_00001'
          i_text_symbol_id    = '002'
          i_language          = 'P'
          i_text              = 'Olá Mundo!'
          i_transport_request = 'TRLK900008'
      ).

    ENDIF.

    IF l_get_method_definition = abap_true.

      l_response = me->get_method_definition( i_class_name  = 'ZCL_CJS_00001'
                                              i_method_name = 'M1' ).

    ENDIF.

    IF l_get_method_implementation = abap_true.

      l_response = me->get_method_implementation( i_class_name  = 'ZCL_CJS_00001'
                                                  i_method_name = 'M1' ).

    ENDIF.

    IF l_get_class_definition = abap_true.

      l_response = me->get_class_definition( i_class_name  = 'ZCL_CJS_00002' ).
*      l_response = me->get_class_definition( i_class_name  = 'ycl_aaic_anthropic' ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
