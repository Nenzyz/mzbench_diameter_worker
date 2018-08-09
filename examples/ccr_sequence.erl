[#{message_name => "CCR",
   message => #{'Auth-Application-Id' => 4,
                'Service-Context-Id' => <<"8.32251@3gpp.org">>,
                'CC-Request-Type' => 1,
                'CC-Request-Number' => 0,
                'Event-Timestamp' => [{{2018,8,8},{13,10,23}}],
                'Subscription-Id' =>
                    [#{'Subscription-Id-Type' => 0,
                       'Subscription-Id-Data' => <<"123456789">>
                      },
                     #{
                       'Subscription-Id-Type' => 1,
                       'Subscription-Id-Data' => <<"987654321">>
                      }]
               }},
 #{message_name => "CCR",
   message => #{'Auth-Application-Id' => 4,
                'Service-Context-Id' => <<"8.32251@3gpp.org">>,
                'CC-Request-Type' => 2,
                'CC-Request-Number' => 1,
                'Event-Timestamp' => [{{2018,8,8},{13,10,24}}],
                'Subscription-Id' =>
                    [#{'Subscription-Id-Type' => 0,
                       'Subscription-Id-Data' => <<"123456789">>
                      },
                     #{
                       'Subscription-Id-Type' => 1,
                       'Subscription-Id-Data' => <<"987654321">>
                      }],
                'Multiple-Services-Credit-Control' =>
                    [#{
                       'Granted-Service-Unit' => [],
                       'Requested-Service-Unit' =>
                           [#{
                              'CC-Time' => [],'CC-Total-Octets' => [],
                              'CC-Input-Octets' => [],'CC-Output-Octets' => [],
                              'CC-Service-Specific-Units' => []}
                           ],
                        'Rating-Group' => [10]
                       }
                    ]

               }},
 #{message_name => "CCR",
   message => #{'Auth-Application-Id' => 4,
                'Service-Context-Id' => <<"8.32251@3gpp.org">>,
                'CC-Request-Type' => 3,
                'CC-Request-Number' => 2,
                'Event-Timestamp' => [{{2018,8,8},{13,10,25}}],
                'Subscription-Id' =>
                    [#{'Subscription-Id-Type' => 0,
                       'Subscription-Id-Data' => <<"123456789">>
                      },
                     #{
                       'Subscription-Id-Type' => 1,
                       'Subscription-Id-Data' => <<"987654321">>
                      }],
                'Multiple-Services-Credit-Control' =>
                    [#{
                       'Used-Service-Unit' =>
                           [#{
                              'CC-Total-Octets' => [1024],
                               'CC-Input-Octets' => [512],
                               'CC-Output-Octets' => [512]
                             }
                           ],
                        'Rating-Group' => [10]
                       }
                    ]
               }}
].
