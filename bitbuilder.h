#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>

#if defined(__clang__)
#define INLINE [[clang::always_inline]]
#elif defined(__GNUC__) || defined(__GNUG__)
#define INLINE [[gnu::always_inline]]
#else
static_assert(false, "bad compiler");
#endif

namespace bitbuilder
{
    namespace str
    {
        template <std::size_t N>
        struct string_literal
        {
            constexpr string_literal(const char (&str)[N]) { std::copy_n(str, N, value); }

            constexpr string_literal(const char (&str)[N - 1], char ch)
            {
                std::copy_n(str, N - 2, value);
                value[N - 2] = ch;
                value[N - 1] = 0;
            }

            constexpr string_literal() { std::fill_n(value, N, 0); }

            constexpr auto begin() const -> const char* { return value; }
            constexpr auto end() const { return begin() + N; }

            constexpr auto begin() -> char* { return value; }
            constexpr auto end() { return begin() + N; }

            constexpr char operator[](size_t index) const { return value[index]; }
            constexpr char& operator[](size_t index) { return value[index]; }

            char value[N];
            inline static constexpr auto size = N - 1;
        };

        template <typename T>
        struct is_strlit : std::false_type
        {
        };
        template <std::size_t N>
        struct is_strlit<string_literal<N>> : std::true_type
        {
        };

        template <typename T>
        concept strlit = is_strlit<std::decay_t<T>>::value;
    } // namespace str

    namespace detail
    {
        enum error_codes
        {
            NONE,
            BAD_PATTERN,
            RANGE_EXPECT_SIMPLE,
            RANGE_EXPECT_DIGIT,
            INCOMPLETE_EXPRESSION,
            SPEC_TOO_LONG,
            SPEC_TOO_SHORT
        };

        template <bool IsInput>
        INLINE constexpr bool is_simple_token(char ch)
        {
            return ('a' <= ch && ch <= 'z') || ('A' <= ch && ch <= 'Z') || (IsInput ? ch == '*' : (ch == '1' || ch == '0'));
        }

        INLINE constexpr bool is_digit(char ch) { return '0' <= ch && ch <= '9'; }

        template <str::string_literal Name, std::integral T, bool IsInput>
        INLINE constexpr error_codes validate_fmt_str()
        {
            size_t bit_length = 0;

            for (size_t i = 0; i < Name.size; i++)
            {
                if (is_simple_token<IsInput>(Name[i]))
                {
                    bit_length++;
                }
                else if (Name[i] == '#')
                {
                    if (i + 2 >= Name.size)
                    {
                        return INCOMPLETE_EXPRESSION;
                    }

                    if (!is_simple_token<IsInput>(Name[i + 1]))
                    {
                        return RANGE_EXPECT_SIMPLE;
                    }

                    if (!is_digit(Name[i + 2]))
                    {
                        return RANGE_EXPECT_DIGIT;
                    }

                    size_t range_length = 0;

                    i = i + 2;
                    while (i < Name.size && is_digit(Name[i]))
                    {
                        range_length = 10 * range_length + (Name[i] - '0');
                        i++;
                    }
                    bit_length += range_length;
                    i--;
                }
                else
                {
                    return BAD_PATTERN;
                }
            }

            if (bit_length > 8 * sizeof(T))
            {
                return SPEC_TOO_LONG;
            }
            else if (bit_length < 8 * sizeof(T))
            {
                return SPEC_TOO_SHORT;
            }

            return NONE;
        }

        template <str::string_literal Name, bool IsInput>
        INLINE constexpr auto fmt_str_bit_length()
        {
            size_t bit_length = 0;

            for (size_t i = 0; i < Name.size; i++)
            {
                if (is_simple_token<IsInput>(Name[i]))
                {
                    bit_length++;
                }
                else if (Name[i] == '#')
                {
                    size_t range_length = 0;

                    i = i + 2;
                    while (i < Name.size && is_digit(Name[i]))
                    {
                        range_length = 10 * range_length + (Name[i] - '0');
                        i++;
                    }

                    bit_length += range_length;
                    i--;
                }
            }

            return bit_length;
        }

        template <str::string_literal Name, size_t N, bool IsInput>
        INLINE constexpr auto normalize_string()
        {
            str::string_literal<N + 1> result{};

            size_t res_idx = 0;

            for (size_t i = 0; i < Name.size; i++)
            {
                if (is_simple_token<IsInput>(Name[i]))
                {
                    result[res_idx++] = Name[i];
                }
                else if (Name[i] == '#')
                {
                    size_t range_length = 0;
                    char ch = Name[i + 1];

                    i = i + 2;
                    while (i < Name.size && is_digit(Name[i]))
                    {
                        range_length = 10 * range_length + (Name[i] - '0');
                        i++;
                    }
                    std::fill_n(result.begin() + res_idx, range_length, ch);
                    res_idx += range_length;
                    i--;
                }
            }

            return result;
        }
    } // namespace detail

    template <str::string_literal Name, std::unsigned_integral T>
    struct bitarg
    {
        T val;
        inline static constexpr auto name = Name;
        using type = T;

        constexpr bitarg(T val) : val(val) {}

    private:
        inline static constexpr auto err_result = detail::validate_fmt_str<Name, T, true>();

        static_assert(
            err_result != detail::BAD_PATTERN,
            "bad bit-pattern: pattern should be [a-zA-Z*] or start with # for range-bit-pattern [related regex: [a-zA-Z*]|(#[a-zA-Z*]\\d+)]");
        static_assert(err_result != detail::RANGE_EXPECT_SIMPLE,
                      "bad range-bit-pattern: the expanded pattern should match [a-zA-Z*], but it does not [related regex: #[a-zA-Z*]\\d+]");
        static_assert(err_result != detail::RANGE_EXPECT_DIGIT,
                      "bad range-bit-pattern: you must specify the length of the bit range [related regex: #[a-zA-Z*]\\d+]");
        static_assert(err_result != detail::INCOMPLETE_EXPRESSION,
                      "bad range-bit-pattern: incomplete expression detected, unexpected EOF [related regex: #[a-zA-Z*]\\d+]");
        static_assert(err_result != detail::SPEC_TOO_LONG, "specified pattern is too large for the type provided");
        static_assert(err_result != detail::SPEC_TOO_SHORT, "specified pattern is too short for the type provided");
    };

    namespace detail
    {
        template <typename T>
        struct is_bitarg : std::false_type
        {
        };

        template <str::string_literal Name, std::integral T>
        struct is_bitarg<bitarg<Name, T>> : std::true_type
        {
        };

        template <typename T>
        concept bigarg_concept = is_bitarg<std::decay_t<T>>::value;

        template <typename T>
        T my_declval();

        template <typename T>
        class uint_bvec
        {
            T value;

        public:
            INLINE constexpr uint_bvec() = default;
            INLINE constexpr uint_bvec(T val) : value(val) {}
            INLINE constexpr bool operator[](size_t index) const { return value & (1 << index); };
            INLINE constexpr void set(size_t index, bool bit) { value = (value & ~(1 << index)) | (uint64_t(bit) << index); };
            template <size_t I>
            INLINE constexpr void set(bool bit)
            {
                value = (value & ~(1 << I)) | (uint64_t(bit) << I);
            };
            INLINE constexpr uint_bvec operator&(uint_bvec rhs) const { return value & (rhs.value); }
            INLINE constexpr uint_bvec operator|(uint_bvec rhs) const { return value | (rhs.value); }

            INLINE constexpr bool any() const { return value; }
            INLINE constexpr auto get() const { return value; }
        };

        template <typename T>
        uint_bvec(T) -> uint_bvec<T>;

        template <str::string_literal Name>
        INLINE constexpr std::array<uint8_t, 26> compute_char_count()
        {
            std::array<uint8_t, 26> ret{};
            for (auto ch : Name)
            {
                if ('A' <= ch && ch <= 'Z')
                    ch = (ch - 'A') + 'a';
                if ('a' <= ch && ch <= 'z')
                    ret[ch - 'a']++;
            }
            return ret;
        }

        template <str::string_literal Name>
        INLINE constexpr uint_bvec<uint32_t> compute_chars_used()
        {
            uint_bvec<uint32_t> ret{};
            for (auto ch : Name)
            {
                if ('A' <= ch && ch <= 'Z')
                    ch = (ch - 'A') + 'a';
                if ('a' <= ch && ch <= 'z')
                    ret.set(ch - 'a', true);
            }

            return ret;
        }

        template <bigarg_concept... Args>
        INLINE constexpr bool check_arg_distinct()
        {
            constexpr uint_bvec<uint32_t> used_ch_list[] = {
                compute_chars_used<normalize_string<Args::name, sizeof(typename Args::type) * 8, true>()>()...};

            uint_bvec<uint32_t> val{};
            for (auto arr : used_ch_list)
            {
                if ((arr & val).any())
                    return false;
                val = val | arr;
            }

            return true;
        }

        template <bigarg_concept... Args>
        INLINE constexpr std::array<uint8_t, 26> compute_total_char_count()
        {
            constexpr std::array<uint8_t, 26> char_count[] = {
                compute_char_count<normalize_string<Args::name, sizeof(typename Args::type) * 8, true>()>()...};

            std::array<uint8_t, 26> val{};
            for (auto arr : char_count)
            {
                for (size_t i = 0; i < arr.size(); i++)
                    val[i] += arr[i];
            }

            return val;
        }

        template <typename T, auto Val>
        struct type_switch_opt
        {
            using type = T;
            inline static constexpr auto value = Val;
        };

        struct no_opt
        {
        };

        template <auto Val, typename... Opts>
        struct type_switch
        {

        private:
            // these functions are never evaluated for anything other than their types
            template <auto _Val, typename _Opt, typename... _Opts>
            static constexpr auto do_switch()
            {
                if constexpr (_Val == _Opt::value)
                    return my_declval<typename _Opt::type>();
                else
                    return do_switch<_Val, _Opts...>();
            }

            template <auto _Val>
            static constexpr auto do_switch()
            {
                return my_declval<no_opt>();
            }

        public:
            using type = decltype(do_switch<Val, Opts...>());
        };

        template <auto Val, typename... Opts>
        using type_switch_t = typename type_switch<Val, Opts...>::type;

        template <char C, bigarg_concept Arg>
        INLINE constexpr auto get_at()
        {
            return +[](size_t index, uint64_t arg) -> bool {
                int64_t ch_i = -1;
                for (size_t i = 0; i < Arg::name.size; i++)
                {
                    if (Arg::name[i] == C)
                    {
                        ch_i++;
                    }

                    if (ch_i == index)
                    {
                        return uint_bvec(arg)[i];
                    }
                }

                return false;
            };
        }

        template <bigarg_concept... Args>
        INLINE constexpr std::array<size_t, 26> compute_ownership_table()
        {
            constexpr uint_bvec<uint32_t> used_ch_list[] = {
                compute_chars_used<normalize_string<Args::name, sizeof(typename Args::type) * 8, true>()>()...};
            std::array<size_t, 26> result{};
            std::fill_n(result.begin(), 26, -1ull);

            size_t index = 0;
            for (auto mask : used_ch_list)
            {
                for (size_t i = 0; i < 26; i++)
                {
                    if (mask[i])
                        result[i] = index;
                }
                index++;
            }

            return result;
        }

        template <bigarg_concept... Args>
        INLINE constexpr std::array<std::array<bool (*)(size_t, uint64_t), sizeof...(Args)>, 26> compute_full_functor_table()
        {
            std::array<std::array<bool (*)(size_t, uint64_t), sizeof...(Args)>, 26> res{};

            // What in the actual fuck
            // ???
            [&]<size_t... Idx>(std::integer_sequence<size_t, Idx...>) {
                (([&]<size_t Val>(std::integral_constant<size_t, Val>) {
                     res[Val] = {get_at<'a' + Val, Args>()...};
                 }(std::integral_constant<size_t, Idx>{})),
                 ...);
            }(std::make_integer_sequence<size_t, 26>{});

            return res;
        }

        template <bigarg_concept... Args>
        INLINE constexpr std::array<bool (*)(size_t, uint64_t), 26> compute_functor_table()
        {
            constexpr auto ownership_table = compute_ownership_table<Args...>();
            constexpr auto fft = compute_full_functor_table<Args...>();
            std::array<bool (*)(size_t, uint64_t), 26> result{};

            for (size_t i = 0; i < 26; i++)
            {
                if (ownership_table[i] != -1ull)
                    result[i] = fft[i][ownership_table[i]];
            }

            return result;
        }

        template <bigarg_concept... Args>
        INLINE constexpr auto compute_value_table(Args... args)
        {
            return std::array<uint64_t, sizeof...(Args)>{(uint64_t)args.val...};
        }

        template <char C, typename T, size_t N, size_t I>
        INLINE constexpr void build_one(detail::uint_bvec<T>& out, std::array<size_t, 26>& index_tab,
                                        const std::array<bool (*)(size_t, uint64_t), 26>& f_tab, const std::array<size_t, 26>& o_tab,
                                        const std::array<uint64_t, N>& v_tab)
        {
            if constexpr (C == '0')
            {
                out.set(I, 0);
            }
            else if constexpr (C == '1')
            {
                out.set(I, 1);
            }
            else
            {
                auto idx = C - 'a';
                auto func = f_tab[idx];
                auto arg_index = o_tab[idx];
                auto value = v_tab[arg_index];

                out.set(I, func(index_tab[idx], value));
                index_tab[idx]++;
            }
        }

        template <str::string_literal Pattern, typename T, size_t N, size_t I = 0>
        INLINE constexpr void build_all(detail::uint_bvec<T>& out, std::array<size_t, 26>& index_tab,
                                        const std::array<bool (*)(size_t, uint64_t), 26>& f_tab, const std::array<size_t, 26>& o_tab,
                                        const std::array<uint64_t, N>& v_tab)
        {
            if constexpr (I == Pattern.size)
            {
                return;
            }
            else
            {
                build_one<Pattern[I], T, N, I>(out, index_tab, f_tab, o_tab, v_tab);
                build_all<Pattern, T, N, I + 1>(out, index_tab, f_tab, o_tab, v_tab);
            }
        }
    } // namespace detail

    template <str::string_literal Pattern, detail::bigarg_concept... Args>
    INLINE constexpr auto build_pattern(Args&&... args)
    {
        constexpr auto real_pat = detail::normalize_string<Pattern, detail::fmt_str_bit_length<Pattern, false>(), false>();

        static_assert(detail::check_arg_distinct<Args...>(), "duplicate identifiers across bit patterns is not allowed");
        constexpr auto char_counts = detail::compute_char_count<real_pat>();
        static_assert(char_counts == detail::compute_total_char_count<Args...>(),
                      "the total amount of bits for each identifier should be equal in input and output");

        using selected_integer_type = detail::type_switch_t<real_pat.size, detail::type_switch_opt<uint8_t, 8>, detail::type_switch_opt<uint16_t, 16>,
                                                            detail::type_switch_opt<uint32_t, 32>, detail::type_switch_opt<uint64_t, 64>>;

        static_assert(!std::same_as<selected_integer_type, detail::no_opt>, "bit pattern must be of length 8, 16, 32, or 64");

        constexpr auto o_tab = detail::compute_ownership_table<Args...>();
        auto v_tab = detail::compute_value_table(args...);
        constexpr auto f_tab = detail::compute_functor_table<Args...>();
        std::array<size_t, 26> index_tab{};
        detail::uint_bvec<selected_integer_type> out{};
        detail::build_all<real_pat>(out, index_tab, f_tab, o_tab, v_tab);
        return out.get();
    }
} // namespace bitbuilder
