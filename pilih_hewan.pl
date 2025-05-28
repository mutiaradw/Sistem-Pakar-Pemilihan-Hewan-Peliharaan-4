% ===================================================================
% SISTEM PAKAR REKOMENDASI HEWAN PELIHARAAN
% MATA KULIAH: KECERDASAN BUATAN
% PROLOG (SWI-PROLOG)
% ===================================================================

:- use_module(library(lists)).

% Deklarasi dinamis untuk menyimpan jawaban pengguna
:- dynamic(yes/1).
:- dynamic(no/1).

% ===================================================================
% DATABASE PENGETAHUAN (ATURAN REKOMENDASI)
% ===================================================================

pet(hamster) :-
    verify(ukuran_tempat_tinggal_apartemen),
    verify(waktu_luang_sedikit),
    verify(anggaran_bulanan_rendah),
    verify(interaksi_rendah),
    verify(komitmen_jangka_pendek),
    verify(ukuran_hewan_kecil).

pet(ikan_cupang) :-
    verify(ukuran_tempat_tinggal_apartemen),
    verify(anggaran_bulanan_rendah),
    verify(toleransi_bising_rendah),
    verify(interaksi_rendah),
    verify(alergi_ringan).

pet(burung_kenari) :-
    verify(ukuran_tempat_tinggal_rumah_kecil),
    verify(toleransi_bising_tinggi),
    verify(anggaran_bulanan_sedang),
    verify(interaksi_sedang).

pet(kelinci) :-
    verify(ada_halaman),
    verify(anggaran_bulanan_sedang),
    verify(ada_anak_kecil),
    verify(interaksi_sedang),
    verify(aktivitas_hewan_tenang).

pet(kucing_domestik) :-
    verify(ukuran_tempat_tinggal_rumah_kecil),
    verify(waktu_luang_sedang),
    verify(anggaran_bulanan_sedang),
    verify(interaksi_tinggi),
    verify(sifat_mandiri).

pet(kucing_domestik) :-
    verify(ukuran_tempat_tinggal_apartemen),
    verify(waktu_luang_sedang),
    verify(grooming_rendah),
    verify(sifat_mandiri).

pet(anjing_golden_retriever) :-
    verify(ukuran_tempat_tinggal_rumah_besar),
    verify(ada_halaman),
    verify(waktu_luang_banyak),
    verify(anggaran_bulanan_tinggi),
    verify(interaksi_tinggi),
    verify(aktivitas_hewan_aktif).

pet(anjing_golden_retriever) :-
    verify(ada_halaman),
    verify(waktu_luang_banyak),
    verify(ada_anak_kecil),
    verify(ukuran_hewan_besar).

pet(anjing_beagle) :-
    verify(ukuran_tempat_tinggal_rumah_kecil),
    verify(ada_halaman),
    verify(waktu_luang_banyak),
    verify(toleransi_bising_tinggi),
    verify(aktivitas_hewan_aktif).

pet(anjing_beagle) :-
    verify(ada_halaman),
    verify(anggaran_bulanan_sedang),
    verify(interaksi_tinggi),
    verify(ukuran_hewan_sedang).

pet(hamster) :-
    verify(pengalaman_pemula),
    verify(anggaran_bulanan_rendah),
    verify(ukuran_hewan_kecil).

pet(ikan_cupang) :-
    verify(pengalaman_pemula),
    verify(anggaran_bulanan_rendah),
    verify(alergi_ringan).

% Jika tidak ada yang cocok
pet(tidak_ada_rekomendasi_spesifik).

% ===================================================================
% MEKANISME INTERAKSI PENGGUNA
% ===================================================================

ask_choice(Question, Options, Chosen) :-
    nl, write(Question), nl,
    display_options(Options, 1),
    write('Pilihan Anda (nomor): '),
    read_line_to_string(user_input, R_Str),
    catch(atom_number(R_Str, R_Num), _, R_Num = -1),
    (nth1(R_Num, Options, Chosen) ->
        assert(yes(Chosen)),
        assert_no_others(Options, Chosen)
    ;   write('Pilihan tidak valid. Harap masukkan nomor yang benar.'), nl,
        ask_choice(Question, Options, Chosen)
    ).

ask_yes_no(Question, Symptom) :-
    format('~w? (y/n) ', [Question]),
    read_line_to_string(user_input, R),
    string_lower(R, LowerR),
    ( (LowerR == "y" ; LowerR == "yes") -> assert(yes(Symptom))
    ; (LowerR == "n" ; LowerR == "no") -> assert(no(Symptom)), fail
    ;   format('Jawaban tidak valid. Harap jawab y atau n.~n'), ask_yes_no(Question, Symptom)
    ).

display_options([], _).
display_options([H|T], N) :-
    atom_string(H, H_String), % Pastikan H adalah atom
    format('   ~d. ~w~n', [N, H_String]),
    N1 is N + 1,
    display_options(T, N1).

assert_no_others([], _).
assert_no_others([H|T], Chosen) :-
    ( H == Chosen -> true ; assert(no(H)) ),
    assert_no_others(T, Chosen).

verify(Criteria) :-
    (yes(Criteria) -> true ;
     no(Criteria) -> fail ;
     (criteria_question(Criteria, Type, Question, Options) ->
         ( (Type == yes_no -> ask_yes_no(Question, Criteria))
         ; (Type == choice -> ask_choice(Question, Options, _))
         ),
         yes(Criteria)
     ;   format('Error: Pertanyaan untuk ~w tidak ditemukan.~n', [Criteria]), fail
     )
    ).

% ===================================================================
% DAFTAR PERTANYAAN (MAPPING KRITERIA KE PERTANYAAN)
% ===================================================================

criteria_question(ukuran_tempat_tinggal_apartemen, choice, 'Apa tipe tempat tinggal Anda?', [ukuran_tempat_tinggal_apartemen, ukuran_tempat_tinggal_rumah_kecil, ukuran_tempat_tinggal_rumah_besar]).
criteria_question(ukuran_tempat_tinggal_rumah_kecil, choice, 'Apa tipe tempat tinggal Anda?', [ukuran_tempat_tinggal_apartemen, ukuran_tempat_tinggal_rumah_kecil, ukuran_tempat_tinggal_rumah_besar]).
criteria_question(ukuran_tempat_tinggal_rumah_besar, choice, 'Apa tipe tempat tinggal Anda?', [ukuran_tempat_tinggal_apartemen, ukuran_tempat_tinggal_rumah_kecil, ukuran_tempat_tinggal_rumah_besar]).

criteria_question(ada_halaman, choice, 'Apakah Anda memiliki halaman?', [ada_halaman, tidak_ada_halaman]).
criteria_question(tidak_ada_halaman, choice, 'Apakah Anda memiliki halaman?', [ada_halaman, tidak_ada_halaman]).

criteria_question(waktu_luang_sedikit, choice, 'Berapa banyak waktu luang Anda per hari untuk hewan?', [waktu_luang_sedikit, waktu_luang_sedang, waktu_luang_banyak]).
criteria_question(waktu_luang_sedang, choice, 'Berapa banyak waktu luang Anda per hari untuk hewan?', [waktu_luang_sedikit, waktu_luang_sedang, waktu_luang_banyak]).
criteria_question(waktu_luang_banyak, choice, 'Berapa banyak waktu luang Anda per hari untuk hewan?', [waktu_luang_sedikit, waktu_luang_sedang, waktu_luang_banyak]).

criteria_question(anggaran_bulanan_rendah, choice, 'Bagaimana anggaran bulanan Anda untuk hewan?', [anggaran_bulanan_rendah, anggaran_bulanan_sedang, anggaran_bulanan_tinggi]).
criteria_question(anggaran_bulanan_sedang, choice, 'Bagaimana anggaran bulanan Anda untuk hewan?', [anggaran_bulanan_rendah, anggaran_bulanan_sedang, anggaran_bulanan_tinggi]).
criteria_question(anggaran_bulanan_tinggi, choice, 'Bagaimana anggaran bulanan Anda untuk hewan?', [anggaran_bulanan_rendah, anggaran_bulanan_sedang, anggaran_bulanan_tinggi]).

criteria_question(alergi_ringan, choice, 'Apakah ada anggota keluarga yang alergi bulu?', [alergi_tidak_ada, alergi_ringan]).
criteria_question(alergi_tidak_ada, choice, 'Apakah ada anggota keluarga yang alergi bulu?', [alergi_tidak_ada, alergi_ringan]).

criteria_question(toleransi_bising_rendah, choice, 'Bagaimana toleransi Anda terhadap kebisingan?', [toleransi_bising_rendah, toleransi_bising_tinggi]).
criteria_question(toleransi_bising_tinggi, choice, 'Bagaimana toleransi Anda terhadap kebisingan?', [toleransi_bising_rendah, toleransi_bising_tinggi]).

criteria_question(interaksi_tinggi, choice, 'Tingkat interaksi apa yang Anda inginkan?', [interaksi_tinggi, interaksi_sedang, interaksi_rendah]).
criteria_question(interaksi_sedang, choice, 'Tingkat interaksi apa yang Anda inginkan?', [interaksi_tinggi, interaksi_sedang, interaksi_rendah]).
criteria_question(interaksi_rendah, choice, 'Tingkat interaksi apa yang Anda inginkan?', [interaksi_tinggi, interaksi_sedang, interaksi_rendah]).

criteria_question(ada_anak_kecil, choice, 'Apakah ada anak kecil di rumah?', [ada_anak_kecil, tidak_ada_anak_kecil]).
criteria_question(tidak_ada_anak_kecil, choice, 'Apakah ada anak kecil di rumah?', [ada_anak_kecil, tidak_ada_anak_kecil]).

criteria_question(komitmen_jangka_pendek, choice, 'Berapa lama komitmen Anda (umur hewan)?', [komitmen_jangka_pendek, komitmen_jangka_panjang]).
criteria_question(komitmen_jangka_panjang, choice, 'Berapa lama komitmen Anda (umur hewan)?', [komitmen_jangka_pendek, komitmen_jangka_panjang]).

criteria_question(aktivitas_hewan_aktif, choice, 'Anda lebih suka hewan yang aktif atau tenang?', [aktivitas_hewan_aktif, aktivitas_hewan_tenang]).
criteria_question(aktivitas_hewan_tenang, choice, 'Anda lebih suka hewan yang aktif atau tenang?', [aktivitas_hewan_aktif, aktivitas_hewan_tenang]).

criteria_question(grooming_rendah, choice, 'Seberapa sering Anda bersedia melakukan grooming?', [grooming_rendah, grooming_tinggi]).
criteria_question(grooming_tinggi, choice, 'Seberapa sering Anda bersedia melakukan grooming?', [grooming_rendah, grooming_tinggi]).

criteria_question(ukuran_hewan_kecil, choice, 'Ukuran hewan peliharaan yang Anda inginkan?', [ukuran_hewan_kecil, ukuran_hewan_sedang, ukuran_hewan_besar]).
criteria_question(ukuran_hewan_sedang, choice, 'Ukuran hewan peliharaan yang Anda inginkan?', [ukuran_hewan_kecil, ukuran_hewan_sedang, ukuran_hewan_besar]).
criteria_question(ukuran_hewan_besar, choice, 'Ukuran hewan peliharaan yang Anda inginkan?', [ukuran_hewan_kecil, ukuran_hewan_sedang, ukuran_hewan_besar]).

criteria_question(sifat_mandiri, yes_no, 'Apakah Anda mencari hewan yang cenderung mandiri', _).
criteria_question(pengalaman_pemula, yes_no, 'Apakah Anda seorang pemula dalam merawat hewan', _).

% ===================================================================
% PROSEDUR UTAMA KONSULTASI
% ===================================================================

go :-
    welcome_message,
    findall(Pet, pet(Pet), PetList),
    process_recommendation(PetList),
    cleanup.

welcome_message :-
    nl,
    write('====================================================='), nl,
    write(' Selamat Datang di Sistem Pakar Pemilihan Hewan     '), nl,
    write('====================================================='), nl,
    write('Jawablah pertanyaan berikut sesuai kondisi dan '), nl,
    write('preferensi Anda.'), nl.

process_recommendation(PetList) :-
    sort(PetList, Sorted),
    ( member(tidak_ada_rekomendasi_spesifik, Sorted), length(Sorted, 1) ->
        output_no_recommendation
    ; delete(Sorted, tidak_ada_rekomendasi_spesifik, CleanList),
      ( CleanList == [] ->
          output_no_recommendation
      ; output_recommendation(CleanList)
      )
    ).

output_no_recommendation :-
    write('====================================================='), nl,
    write(' HASIL REKOMENDASI:'), nl,
    write('====================================================='), nl,
    write('Berdasarkan jawaban Anda, sistem tidak dapat menemukan '), nl,
    write('rekomendasi hewan peliharaan yang sangat spesifik dari '), nl,
    write('daftar kami. Mungkin Anda bisa mencoba kombinasi lain '), nl,
    write('atau mencari informasi lebih lanjut.'), nl,
    write('====================================================='), nl.

output_recommendation(PetList) :-
    write('====================================================='), nl,
    write(' HASIL REKOMENDASI:'), nl,
    write('====================================================='), nl,
    write('Berdasarkan jawaban Anda, hewan peliharaan yang'), nl,
    write('mungkin cocok untuk Anda adalah:'), nl, nl,
    display_pets(PetList),
    write('====================================================='), nl,
    write('PERHATIAN: Ini adalah rekomendasi awal. Lakukan riset'), nl,
    write('lebih mendalam tentang kebutuhan spesifik setiap hewan'), nl,
    write('sebelum membuat keputusan akhir.'), nl,
    write('====================================================='), nl.

display_pets([]).
display_pets([H|T]) :-
    format('* ~w~n', [H]),
    pet_description(H),
    pet_care_notes(H), nl,
    display_pets(T).

cleanup :-
    retractall(yes(_)),
    retractall(no(_)).

% ===================================================================
% PENJELASAN DAN CATATAN PERAWATAN (DATABASE FAKTA)
% ===================================================================

pet_description(hamster) :- write('  Deskripsi: Hewan pengerat kecil, aktif di malam hari, perawatannya relatif mudah.').
pet_description(ikan_cupang) :- write('  Deskripsi: Ikan air tawar yang cantik, tidak butuh ruang besar, cenderung soliter.').
pet_description(burung_kenari) :- write('  Deskripsi: Burung penyanyi yang indah, cocok untuk menghidupkan suasana rumah.').
pet_description(kelinci) :- write('  Deskripsi: Hewan sosial yang lembut, butuh ruang gerak dan diet khusus (hay).').
pet_description(kucing_domestik) :- write('  Deskripsi: Hewan independen namun penyayang, banyak variasi sifat, cocok untuk indoor.').
pet_description(anjing_golden_retriever) :- write('  Deskripsi: Anjing keluarga yang setia, ramah, cerdas, dan butuh banyak latihan.').
pet_description(anjing_beagle) :- write('  Deskripsi: Anjing pemburu yang ceria, penasaran, aktif, dan memiliki suara khas.').
pet_description(_) :- !.

pet_care_notes(hamster) :- write('  Catatan: Butuh kandang yang aman, makanan biji-bijian, dan mainan. Umur pendek (2-3 tahun).').
pet_care_notes(ikan_cupang) :- write('  Catatan: Butuh akuarium minimal 5 liter dengan air bersih, jangan digabung dengan cupang jantan lain.').
pet_care_notes(burung_kenari) :- write('  Catatan: Butuh sangkar yang luas, pakan berkualitas, dan kebersihan kandang terjaga.').
pet_care_notes(kelinci) :- write('  Catatan: Perlu kandang besar/area bermain, 80% dietnya hay, rentan stres.').
pet_care_notes(kucing_domestik) :- write('  Catatan: Butuh vaksinasi rutin, litter box bersih, mainan, dan interaksi. Umur bisa 15+ tahun.').
pet_care_notes(anjing_golden_retriever) :- write('  Catatan: Butuh latihan fisik dan mental harian, grooming rutin, rentan masalah sendi.').
pet_care_notes(anjing_beagle) :- write('  Catatan: Butuh latihan intensif, pagar yang aman (suka kabur), cenderung berisik.').
pet_care_notes(_) :- !.
