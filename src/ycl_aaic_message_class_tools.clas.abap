CLASS ycl_aaic_message_class_tools DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun.

    METHODS create_message
      IMPORTING
                i_message_class     TYPE bapiret2-id
                i_message_number    TYPE bapiret2-number
                i_message_text      TYPE bapiret2-message
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS read_message
      IMPORTING
                i_message_class   TYPE bapiret2-id
                i_message_number  TYPE bapiret2-number
      RETURNING VALUE(r_response) TYPE string.

    METHODS update_message
      IMPORTING
                i_message_class     TYPE bapiret2-id
                i_message_number    TYPE bapiret2-number
                i_message_text      TYPE bapiret2-message
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS delete_message
      IMPORTING
                i_message_class     TYPE bapiret2-id
                i_message_number    TYPE bapiret2-number
                i_transport_request TYPE yde_aaic_fc_transport_request
      RETURNING VALUE(r_response)   TYPE string.

    METHODS read_all_messages
      IMPORTING
                i_message_class   TYPE bapiret2-id
      RETURNING VALUE(r_response) TYPE string.

    METHODS set_translation
      IMPORTING
                i_message_class     TYPE bapiret2-id
                i_message_number    TYPE bapiret2-number
                i_transport_request TYPE yde_aaic_fc_transport_request
                i_language          TYPE spras
                i_message_text      TYPE bapiret2-message
      RETURNING VALUE(r_response)   TYPE string.

    METHODS get_translation
      IMPORTING
                i_message_class   TYPE bapiret2-id
                i_message_number  TYPE bapiret2-number OPTIONAL
                i_language        TYPE spras
      RETURNING VALUE(r_response) TYPE string.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ycl_aaic_message_class_tools IMPLEMENTATION.

  METHOD create_message.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-msag->create_patch_operation( ).

    DATA(lo_object) = lo_patch_operation->add_object( l_message_class_name ).

    lo_object->for-insert->add_message( i_message_number )->set_short_text( CONV #( i_message_text ) ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Message `{ i_message_number }` created successfully!|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error: the message `{ i_message_number }` was not created.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->msag.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD read_message.

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(lo_message_class) = xco_cp_abap_repository=>object->msag->for( l_message_class_name ).

    TRY.

        LOOP AT lo_message_class->messages->all->get( ) INTO DATA(lo_message).

          IF lo_message->number <> i_message_number.
            CONTINUE.
          ENDIF.

          DATA(ls_message) = lo_message->content( )->get( ).

          r_response = |Message `{ lo_message->number }`: { ls_message-short_text }|.

        ENDLOOP.

        IF r_response IS INITIAL.

          r_response = |Message `{ i_message_number }` not found in the message class **{ l_message_class_name }**.|.

        ENDIF.

      CATCH cx_xco_ar_existence_exception ##NO_HANDLER.

        r_response = |The message class **{ l_message_class_name }** does not exist.|.

    ENDTRY.

  ENDMETHOD.

  METHOD update_message.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-msag->create_patch_operation( ).

    DATA(lo_object) = lo_patch_operation->add_object( l_message_class_name ).

    lo_object->for-update->message( i_message_number )->set_short_text( CONV #( i_message_text ) ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Message `{ i_message_number }` updated successfully!|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error: the message `{ i_message_number }` was not updated.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->msag.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD delete_message.

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(lo_patch_operation) = xco_cp_generation=>environment->dev_system( l_transport_request )->for-msag->create_patch_operation( ).

    DATA(lo_object) = lo_patch_operation->add_object( l_message_class_name ).

    lo_object->for-delete->add_message( i_message_number ).

    TRY.

        lo_patch_operation->execute( ).

        r_response = |Message `{ i_message_number }` deleted successfully!|.

      CATCH cx_xco_gen_patch_exception INTO DATA(lo_cx_xco_gen_patch_exception).

        r_response = |Error: the message `{ i_message_number }` was not deleted.|.

        DATA(lo_findings) = lo_cx_xco_gen_patch_exception->findings->for->msag.

        DATA(lt_findings) = lo_findings->get( ).

        LOOP AT lt_findings ASSIGNING FIELD-SYMBOL(<ls_finding>).

          IF r_response IS NOT INITIAL.
            r_response = r_response && cl_abap_char_utilities=>newline.
          ENDIF.

          LOOP AT <ls_finding>->message->if_xco_news~get_messages( ) ASSIGNING FIELD-SYMBOL(<lo_message>).

            r_response = r_response && <lo_message>->get_text( ).

          ENDLOOP.

        ENDLOOP.

    ENDTRY.

  ENDMETHOD.

  METHOD read_all_messages.

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(lo_message_class) = xco_cp_abap_repository=>object->msag->for( l_message_class_name ).

    r_response = |Messages of message class **{ l_message_class_name }**:|.

    TRY.

        LOOP AT lo_message_class->messages->all->get( ) INTO DATA(lo_message).

          DATA(ls_message) = lo_message->content( )->get( ).

          r_response = |{ r_response }{ cl_abap_char_utilities=>newline } - Message `{ lo_message->number }`: { ls_message-short_text }|.

        ENDLOOP.

      CATCH cx_xco_ar_existence_exception ##NO_HANDLER.

        r_response = |The message class **{ l_message_class_name }** does not exist.|.

    ENDTRY.

  ENDMETHOD.

  METHOD get_translation.

    DATA l_messages_not_translated TYPE string.

    CLEAR r_response.

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(l_language) = i_language.

    l_language = to_upper( l_language ).

    DATA(lo_language) = xco_cp=>language( l_language ).

    DATA(lo_message_class) = xco_cp_abap_repository=>object->msag->for( l_message_class_name ).

    TRY.

        LOOP AT lo_message_class->messages->all->get( ) INTO DATA(lo_message).

          IF i_message_number IS SUPPLIED AND lo_message->number <> i_message_number.
            CONTINUE.
          ENDIF.

          IF r_response IS INITIAL.
            r_response = |Message(s) of message class **{ l_message_class_name }** in language `{ i_language }`:|.
          ENDIF.

          DATA(lo_target) = xco_cp_i18n=>target->message_class->message(
                        iv_message_class_name = l_message_class_name
                        iv_message_number     = lo_message->number
                      ).

          DATA(lo_message_short_text) = xco_cp_message_class=>text_attribute->message_short_text.

          TRY.

              " Read the translation.
              DATA(lo_translation) = lo_target->get_translation(
                io_language        = lo_language
                it_text_attributes = VALUE #( ( lo_message_short_text ) )
              ).

              LOOP AT lo_translation->texts INTO DATA(lo_text).

                IF r_response IS NOT INITIAL.
                  r_response = r_response && cl_abap_char_utilities=>newline.
                ENDIF.

                r_response = |{ r_response }{ cl_abap_char_utilities=>newline } - Message `{ lo_message->number }`: { lo_message_short_text->if_xco_i18n_text_attribute~get_string_for_text( lo_text->value ) }|.

              ENDLOOP.

            CATCH cx_xco_runtime_exception ##NO_HANDLER.

              IF l_messages_not_translated IS INITIAL.
                l_messages_not_translated = |`{ lo_message->number }`|.
              ELSE.
                l_messages_not_translated = |{ l_messages_not_translated }, `{ lo_message->number }`|.
              ENDIF.

              CONTINUE.

          ENDTRY.

        ENDLOOP.

        IF l_messages_not_translated IS NOT INITIAL.

          r_response = |{ r_response }{ cl_abap_char_utilities=>newline }{ cl_abap_char_utilities=>newline }|.

          r_response = |{ r_response }Messages **without translation** to language `{ i_language }`: { l_messages_not_translated }|.

        ENDIF.

      CATCH cx_xco_ar_existence_exception ##NO_HANDLER.

        r_response = |The message class **{ l_message_class_name }** does not exist.|.

    ENDTRY.

  ENDMETHOD.

  METHOD set_translation.

    DATA(l_message_class_name) = CONV bapiret2-id( condense( to_upper( i_message_class ) ) ).

    DATA(l_transport_request) = CONV sxco_transport( condense( to_upper( i_transport_request ) ) ).

    DATA(lo_transport_request) = xco_cp_cts=>transport->for( iv_transport = l_transport_request ).

    DATA(l_language) = i_language.

    l_language = to_upper( l_language ).

    DATA(lo_language) = xco_cp=>language( l_language ).

    DATA(lo_message_short_text) = xco_cp_message_class=>text_attribute->message_short_text.

    DATA(lo_message_text) = lo_message_short_text->create_text( xco_cp=>string( i_message_text ) ).

    DATA(lo_target) = xco_cp_i18n=>target->message_class->message(
                        iv_message_class_name = l_message_class_name
                        iv_message_number     = i_message_number
                      ).

    TRY.

        " Set the translation.
        lo_target->set_translation(
          it_texts           = VALUE #( ( lo_message_text ) )
          io_language        = lo_language
          io_change_scenario = lo_transport_request
        ).

        r_response = |Message `{ i_message_number }` of message class **{ l_message_class_name }** translated successfully!|.

      CATCH cx_xco_runtime_exception INTO DATA(lo_cx_xco_runtime_exception).

        r_response = |Error! Message `{ i_message_number }` of message class **{ l_message_class_name }** was not translated.{ cl_abap_char_utilities=>newline }|.
        r_response = |{ r_response }{ lo_cx_xco_runtime_exception->get_text( ) }|.

    ENDTRY.

  ENDMETHOD.

  METHOD if_oo_adt_classrun~main.

    DATA l_response TYPE string.

    DATA(l_create) = abap_false.
    DATA(l_read) = abap_false.
    DATA(l_update) = abap_false.
    DATA(l_delete) = abap_false.
    DATA(l_read_all) = abap_false.
    DATA(l_set_translation) = abap_false.
    DATA(l_get_translation) = abap_true.

    "Create Message
    IF l_create = abap_true.

      l_response = me->create_message(
        EXPORTING
          i_message_class     = 'ZCHRJS'
          i_message_number    = '001'
          i_message_text      = 'Message 001'
          i_transport_request = 'TRLK900028'
      ).

    ENDIF.

    "Read message
    IF l_read = abap_true.

      l_response = me->read_message(
        EXPORTING
          i_message_class  = 'ZCHRJS'
          i_message_number = '002'
      ).

    ENDIF.

    "Read all messages
    IF l_read_all = abap_true.

      l_response = me->read_all_messages(
        EXPORTING
          i_message_class = 'ZCHRJS'
      ).

    ENDIF.

    "Update message
    IF l_update = abap_true.

      l_response = me->update_message(
        EXPORTING
          i_message_class     = 'ZCHRJS'
          i_message_number    = '001'
          i_message_text      = 'Message 001...'
          i_transport_request = 'TRLK900028'
      ).

    ENDIF.

    "Delete message
    IF l_delete = abap_true.

      l_response = me->delete_message(
        EXPORTING
          i_message_class     = 'ZCHRJS'
          i_message_number    = '099'
          i_transport_request = 'TRLK900028'
      ).

    ENDIF.

    "Translate message
    IF l_set_translation = abap_true.

      l_response = me->set_translation(
        EXPORTING
          i_message_class     = 'ZCHRJS'
          i_message_number    = '001'
          i_transport_request = 'TRLK900028'
          i_language          = 'P'
          i_message_text      = 'Mensagem 001.'
      ).

    ENDIF.

    "Get message translation
    IF l_get_translation = abap_true.

      l_response = me->get_translation(
        EXPORTING
          i_message_class  = 'ZCHRJS'
*          i_message_number = '001'
          i_language       = 'P'
      ).

    ENDIF.

    out->write( l_response ).

  ENDMETHOD.

ENDCLASS.
